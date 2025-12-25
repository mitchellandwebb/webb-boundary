module Webb.Boundary.Analyzer.TypeCheck where

import Webb.Boundary.Prelude
import Webb.Boundary.Data.Tree
import Webb.Boundary.Analyzer.TypeSymbols

import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.State (StateT, evalStateT)
import Data.Array as A
import Data.Either (Either)
import Data.Foldable (for_)
import Data.Maybe (fromMaybe)
import Effect.Aff (Aff, throwError)
import Effect.Aff.Class (class MonadAff, liftAff)
import Webb.Boundary.Data.Alias (Alias)
import Webb.Boundary.Data.Alias as Alias
import Webb.Boundary.Data.Method (Method)
import Webb.Boundary.Data.Method as Method
import Webb.Boundary.Data.Param (Param)
import Webb.Boundary.Data.Param as Param
import Webb.Boundary.Data.SymbolTable (SymbolTable)
import Webb.Boundary.Data.SymbolTable as SymbolTable
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
  
type Prog = ExceptT (Array String) (StateT Env Aff)

run :: forall m. MonadAff m => Env -> Prog Unit -> m Unit
run env prog = liftAff do void $ prog # runExceptT >>> flip evalStateT env

eval :: forall m a. MonadAff m => Env -> Prog a -> m (Either (Array String) a)
eval env prog = liftAff do prog # runExceptT >>> flip evalStateT env

runTypeCheck :: SymbolTable -> Tree -> Aff (Either (Array String) Unit)
runTypeCheck table tree = do 
  errors <- newArray
  let 
    env = { symbols: table, tree, errors } :: Env
    prog = do
      abort $ checkCircular 
      abort $ checkTypes 
      abort $ checkIllegal 
  prog # eval env

  where 
  abort :: Prog Unit -> Prog Unit
  abort prog = do
    prog
    env <- mread
    len <- Arr.length env.errors
    if len <= 0 then
      pure unit
    else do
      errs <- aread env.errors
      throwError errs
      
checkCircular :: Prog Unit
checkCircular = do 
  this <- mread
  liftAff do
    allAliases this.tree $ \alias -> 
      run this (noCircularAlias alias)

checkTypes :: Prog Unit
checkTypes = do 
  this <- mread
  liftAff do 
    allParams this.tree $ \param -> 
      run this $ typeCheckParam param

checkIllegal :: Prog Unit
checkIllegal = do 
  this <- mread
  liftAff do
    allMethods this.tree $ \method -> 
      run this (illegalMethod method)

addError :: String -> Prog Unit
addError msg = do 
  env <- mread
  Arr.addLast env.errors msg
  
expect :: Boolean -> String -> Prog Unit
expect success msg = do unless success do addError msg

resolve :: String -> Prog String
resolve name = do 
  env <- mread
  let mname = SymbolTable.resolveToHigherType name env.symbols
  pure $ fromMaybe "unknown" mname

-- If we detect a circular relation where the param refers back to the
-- alias symbol in _any_ parts of the param (name or args), add an error.
checkCircularAlias :: String -> Prog Unit
checkCircularAlias name = do
  this <- mread
  expect (not $ SymbolTable.isCircularAlias name this.symbols)
    $ "Type alias refers back to itself: " <> name


-- Alias definitions must not be circular. They cannot eventually refer
-- back to themselves in their concrete type, in _any_ of the parameters.
-- Because if they do, we can't successfully write out the type.
noCircularAlias :: Alias -> Prog Unit
noCircularAlias al = checkCircularAlias $ Alias.name al

-- We check each parameter for existence and completeness
typeCheckParam :: Param -> Prog Unit
typeCheckParam param = do
  let 
    name = Param.name param
    args = Param.args param
    
  nameExists name -- Does it refer to a valid existing type?
  argCountIsCorrect name args -- Is that type completed by the args?
  
  -- Note that any child params are handled by the visitor. We don't need
  -- to recurse here.
  
  where
  nameExists name = do
    self <- mread
    let env = self
    expect (SymbolTable.member name env.symbols) $
      "Unknown type: " <> name
    
  argCountIsCorrect name args = do
    env <- mread
    when (SymbolTable.isProduct name env.symbols) do
      let expected = SymbolTable.argCount name env.symbols
          actual = A.length args           
      expect (expected == actual) $
        "Expected " <> show expected <> 
        " type arguments, but got " <> show actual
        
-- We check each method for correct usage of types. Some usages
-- are currently illegal.
illegalMethod :: Method -> Prog Unit
illegalMethod m = do 
  let args = Method.firstParams m
      return = Method.returnParam m
  
  for_ args noEffectOrAff

  requireEffectOrAff return
  for_ (Param.args return) noEffectOrAff
  
  where
  noEffectOrAff param = do 
    -- Final resolution of the type name to a non-alias must not be
    -- Effect or Aff
    name <- resolve $ Param.name param
    expect (name /= "Effect") 
      "Cannot use Effect here"
    expect (name /= "Aff") 
      "Cannot use Aff here"
    
    -- Recursively, even the type arguments in a function parameter cannot 
    -- use Effect or Aff. It is exclusive to the Return value.
    for_ (Param.args param) noEffectOrAff
    
  requireEffectOrAff param = do
    -- Final resolution of the type name to a non-alias must be Effect or Aff
    name <- resolve $ Param.name param
    expect (name == "Effect" || name == "Aff")
      "Function return type must be Aff or Effect"