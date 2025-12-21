module Webb.Boundary.TypeCheck where

import Webb.Boundary.Prelude
import Webb.Boundary.Tree
import Webb.Boundary.TypeSymbols

import Data.Array as A
import Data.Foldable (for_)
import Data.Map as Map
import Data.Newtype (unwrap)
import Effect.Aff (Aff)
import Effect.Class (class MonadEffect, liftEffect)
import Webb.Boundary.Parser as P
import Webb.Boundary.TypeSymbols as TS
import Webb.Monad.Prelude (forceMaybe')
import Webb.Stateful.ArrayColl (ArrayColl)
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


-- TODO. Type-checking proceeds in a few phases.
-- First, check aliases for circularity. Then check params for
-- completeness. And finally, check functions for correct Aff/Effect usage.
-- We stop immediately if errors are detected at a phase.

type Env = 
  { symbols :: SymbolTable
  , errors :: ArrayColl String
  }

newtype CheckVisitor = CV Env

newVisitor :: Env -> Aff CheckVisitor
newVisitor env = do pure $ CV env

addError :: CheckVisitor -> String -> Aff Unit
addError (CV env) msg = Arr.addLast env.errors msg

expect :: CheckVisitor -> Boolean -> String -> Aff Unit
expect self success msg = unless success do addError self msg

resolve :: CheckVisitor -> String -> String 
resolve (CV env) name = TS.resolve name env.symbols

-- If we detect a circular relation where the param refers back to the
-- alias symbol in _any_ parts of the param (name or args), add an error.
checkCircularAlias :: CheckVisitor -> String -> P.Param -> Aff Unit
checkCircularAlias this@(CV env) name p = do
  expect this 
    (not $ refersToSymbol name p env.symbols) 
    $ "Type alias refers back to itself: " <> name

instance Visitor CheckVisitor where
  -- We check each parameter for existence and completeness
  param self@(CV env) wrapped = do
    let 
      p = unwrap wrapped 
      name = p.name.string
      args = p.args
      
    nameExists name -- Does it refer to a valid existing type?
    argCountIsCorrect name args -- Is that type completed by the args?
    
    where
    nameExists name = do
      expect self (Map.member name env.symbols) $
        "Unknown type: " <> name
      
    argCountIsCorrect name args = do
      when (isProduct name env.symbols) do
        let expected = argCount name env.symbols
            actual = A.length args           
        expect self (expected == actual) $
          "Expected " <> show expected <> 
          " type arguments, but got " <> show actual

  -- We check each method for correct usage of types. Some usages
  -- are currently illegal.
  method this@(CV _env) m = do 
    args <- methodParams m
    return <- methodReturn m
    
    for_ args noEffectOrAff
    requireEffectOrAff return
    
    where
    noEffectOrAff w = do 
      -- Final resolution of the type name to a non-alias must not be
      -- Effect or Aff
      let p = unwrap w
      expect this (resolve this p.name.string /= "Effect") 
        "Cannot use Effect in a function parameter"
      expect this (resolve this p.name.string /= "Aff") 
        "Cannot use Aff in a function parameter"
      
      -- Recursively, even the type arguments in a function parameter cannot 
      -- use Effect or Aff. It is exclusive to the Return value.
      for_ p.args noEffectOrAff
      
    requireEffectOrAff w = do
      -- Final resolution of the type name to a non-alias must be Effect or Aff
      let p = unwrap w
      expect this 
        (resolve this p.name.string == "Effect" || 
          resolve this p.name.string == "Aff"
        ) $ "Function return type must be Aff or Effect"

  -- Alias definitions must not be circular. They cannot eventually refer
  -- back to themselves in their concrete type, in _any_ of the parameters.
  -- Because if they do, we can't successfully write out the type.
  alias this al = do 
    let name = al.name.string
    case al.target of 
      P.AliasedParam p -> do
        checkCircularAlias this name p
      P.AliasedMap m -> do
        let params = Map.values m
        for_ params \p -> do
          checkCircularAlias this name p
      
  -- Parameter-checking is covered by 'param' alone. Nothing else is needed
  -- for our use case.
  boundary = defaultBoundary
  typeMap = defaultTypeMap


methodParams :: forall m. MonadEffect m => P.Method -> m (Array P.Param)
methodParams method = liftEffect do
  let minit = A.init method.params
  forceMaybe' "No function parameters were provided" minit
  
methodReturn :: forall m. MonadEffect m => P.Method -> m P.Param
methodReturn method = liftEffect do
  let mlast = A.last method.params
  forceMaybe' "No function parameters were provided" mlast