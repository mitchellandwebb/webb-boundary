module Webb.Boundary.TypeCheck where

import Webb.Boundary.Prelude
import Webb.Boundary.Tree
import Webb.Boundary.TypeSymbols

import Control.Monad.Except (ExceptT, lift, runExceptT)
import Control.Monad.State (StateT, evalStateT)
import Data.Array as A
import Data.Either (Either)
import Data.Foldable (for_)
import Data.Map as Map
import Data.Newtype (unwrap)
import Effect.Aff (Aff, throwError)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Webb.Boundary.Parser as P
import Webb.Boundary.TypeSymbols as TS
import Webb.Monad.Prelude (forceMaybe')
import Webb.Stateful.ArrayColl (ArrayColl, newArray)
import Webb.Stateful.ArrayColl as Arr


{- Once all global type symbols have been defined, we can start checking
  whether all type usages are actually correct. This means we need to check:

  - Is each type usage complete, whether in Alias, Type Map, or anywhere?
  - Does each function in a boundary express types correctly?

  In each case, we are checking whether a usage like `Array Int String` refers to
  defined symbols, and uses those symbols relatively correctly within the type
  definition, since the only operation here is type composition -- we are
  not checking operation type results, or inferring types -- we have no need to
  do that.

  Note that the boundary table may still be useful, to go through all boundaries
  and write them and their functions to file with ease. And if we had code that
  used the boundary table, the code would have used the boundary table to quickly
  look up the type definition of the boundary's functions.
  
  To perform the parameter check, we _don't_ need to iterate over TypeMap, Alias,
  and Boundary. Each instance of 'Param' is a place where we can check the usage.
-}


-- In the context of this local state, run the following code.
type Env = 
  { symbols :: SymbolTable
  , errors :: ArrayColl String
  , tree :: Tree
  }
  
type Check = StateT Env Aff

run :: forall m. MonadAff m => Env -> Check Unit -> m Unit
run env prog = liftAff do evalStateT prog env

eval :: forall m a. MonadAff m => Env -> Check a -> m a
eval env prog = liftAff do evalStateT prog env

runTypeCheck :: SymbolTable -> Tree -> Aff (Either (Array String) Unit)
runTypeCheck table tree = do 
  errors <- newArray
  let 
    env = { symbols: table, tree, errors } :: Env
    prog = do
      abort $ checkCircular 
      abort $ checkTypes 
      abort $ checkIllegal 
      
  prog # runExceptT >>> eval env

  where 
  abort :: Check Unit -> ExceptT (Array String) Check Unit
  abort prog = do
    lift prog
    env <- mread
    len <- Arr.length env.errors
    if len <= 0 then
      pure unit
    else do
      errs <- aread env.errors
      throwError errs
      
checkCircular :: Check Unit
checkCircular = do 
  this <- mread
  liftAff do
    allAliases this.tree $ \alias -> 
      run this (noCircularAlias alias)

checkTypes :: Check Unit
checkTypes = do 
  this <- mread
  liftAff do 
    allParams this.tree $ \param -> 
      run this $ typeCheckParam param

checkIllegal :: Check Unit
checkIllegal = do 
  this <- mread
  liftAff do
    allMethods this.tree $ \method -> 
      run this (illegalMethod method)

addError :: String -> Check Unit
addError msg = do 
  env <- mread
  Arr.addLast env.errors msg
  
expect :: Boolean -> String -> Check Unit
expect success msg = do unless success do addError msg

resolve :: String -> Check String
resolve name = do 
  env <- mread
  pure $ TS.resolve name env.symbols

-- If we detect a circular relation where the param refers back to the
-- alias symbol in _any_ parts of the param (name or args), add an error.
checkCircularAlias :: String -> P.Param -> Check Unit
checkCircularAlias name p = do
  this <- mread
  expect (not $ refersToSymbol name p this.symbols) 
    $ "Type alias refers back to itself: " <> name


-- Alias definitions must not be circular. They cannot eventually refer
-- back to themselves in their concrete type, in _any_ of the parameters.
-- Because if they do, we can't successfully write out the type.
noCircularAlias :: P.Alias -> Check Unit
noCircularAlias al = do 
  let name = al.name.string
  case al.target of 
    P.AliasedParam p -> do
      checkCircularAlias name p
    P.AliasedMap m -> do
      let params = Map.values m
      for_ params \p -> do
        checkCircularAlias name p

-- We check each parameter for existence and completeness
typeCheckParam :: P.Param -> Check Unit
typeCheckParam wrapped = do
  let 
    p = unwrap wrapped 
    name = p.name.string
    args = p.args
    
  nameExists name -- Does it refer to a valid existing type?
  argCountIsCorrect name args -- Is that type completed by the args?
  
  -- Note that any child params are handled by the visitor. We don't need
  -- to recurse here.
  
  where
  nameExists name = do
    self <- mread
    let env = self
    expect (Map.member name env.symbols) $
      "Unknown type: " <> name
    
  argCountIsCorrect name args = do
    env <- mread
    when (isProduct name env.symbols) do
      let expected = argCount name env.symbols
          actual = A.length args           
      expect (expected == actual) $
        "Expected " <> show expected <> 
        " type arguments, but got " <> show actual
        
-- We check each method for correct usage of types. Some usages
-- are currently illegal.
illegalMethod :: P.Method -> Check Unit
illegalMethod m = do 
  args <- methodParams m
  return <- methodReturn m
  
  for_ args noEffectOrAff
  requireEffectOrAff return
  
  where
  noEffectOrAff w = do 
    -- Final resolution of the type name to a non-alias must not be
    -- Effect or Aff
    let p = unwrap w
    name <- resolve p.name.string
    expect (name /= "Effect") 
      "Cannot use Effect in a function parameter"
    expect (name /= "Aff") 
      "Cannot use Aff in a function parameter"
    
    -- Recursively, even the type arguments in a function parameter cannot 
    -- use Effect or Aff. It is exclusive to the Return value.
    for_ p.args noEffectOrAff
    
  requireEffectOrAff w = do
    -- Final resolution of the type name to a non-alias must be Effect or Aff
    let p = unwrap w
    name <- resolve p.name.string
    expect (name == "Effect" || name == "Aff")
      "Function return type must be Aff or Effect"

methodParams :: forall m. MonadEffect m => P.Method -> m (Array P.Param)
methodParams method = liftEffect do
  let minit = A.init method.params
  forceMaybe' "No function parameters were provided" minit
  
methodReturn :: forall m. MonadEffect m => P.Method -> m P.Param
methodReturn method = liftEffect do
  let mlast = A.last method.params
  forceMaybe' "No function parameters were provided" mlast