module Webb.Boundary.Gen where

import Webb.Boundary.Prelude
import Webb.Boundary.Tree

import Data.Array as Array
import Data.Either (Either)
import Data.Foldable (for_)
import Data.Map (Map)
import Data.Map as Map
import Data.Set as Set
import Effect.Aff (Aff, throwError)
import Webb.Boundary.BoundarySymbols (BoundaryTable)
import Webb.Boundary.Data.Alias as Alias
import Webb.Boundary.Data.Boundary as Bound
import Webb.Boundary.Data.Method as Method
import Webb.Boundary.Data.Param as Param
import Webb.Boundary.Data.SymbolTable (SymbolTable)
import Webb.Boundary.Data.SymbolTable as SymbolTable
import Webb.Boundary.Data.Token as Token
import Webb.Boundary.Data.TypeMap as TypeMap
import Webb.Monad.Prelude (forceMaybe')
import Webb.Stateful (localEffect)
import Webb.Stateful.ArrayColl (ArrayColl, newArray)
import Webb.Stateful.ArrayColl as Arr


{- Define the data structures and functions that will be used by generators to build code.
-}

type GenEnv = 
  { symbols :: SymbolTable
  , boundaries :: BoundaryTable
  , writable ::
    { aliases :: Array GenAlias
    , boundaries :: Array GenBoundary
    }
  }
  
-- Either we are ready to generate, or we got errors
buildEnv :: String -> Aff (Either (Array String) GenEnv)
buildEnv file = do
  -- TODO -- we parse the file, perform checks on it, and then get the writable
  -- aliases and boundaries
  pure $ throwError []


type GenAlias = String /\ GenAliasTarget

-- Is this useful? Only sort-of, because parameters will refer to aliases ... which
-- will take us back to normal RecordMaps.
data GenAliasTarget = AliasMap GenTypeMap | AliasParam Param.Param

type GenBoundary = String /\ Array FnDef

type GenTypeMap = Map String Param.Param

type FnDef = 
  { name :: String
  , args :: Array Param.Param
  , return :: Param.Param
  }
  
getAliases :: Tree -> Aff (Array GenAlias)
getAliases tree = do
  arr <- newArray
  allAliases tree \alias -> addAlias arr alias
  aread arr
  
  where
  addAlias arr alias = do
    let name = Alias.name alias
    case Alias.target alias of
      Alias.AliasedParam p -> do
        Arr.addLast arr $ name /\ AliasParam p
      Alias.AliasedMap m -> do
        Arr.addLast arr $ name /\ AliasMap (convert m)
        
  convert map = let
    pairs = TypeMap.pairs map
    converted = pairs <#> uncurry \token param -> Token.text token /\ param
    in Map.fromFoldable converted

getBoundaries :: Tree -> Aff (Array GenBoundary)
getBoundaries tree = do
  boundArray <- newArray
  allBoundaries tree \b -> addBound boundArray b
  aread boundArray
  
  where
  addBound :: ArrayColl GenBoundary -> Bound.Boundary -> _
  addBound boundArray b = do
    let name = Bound.name b
    defArray <- newArray
    for_ (Bound.methods b) \m -> addDef defArray m
    
    defs <- aread defArray
    Arr.addLast boundArray $ name /\ defs
    
  addDef defArray m = do
    let name = Method.name m
        args = Method.firstParams m
        return = Method.returnParam m
    Arr.addLast defArray { name, args, return }

-- Sort the aliases in order of their dependency on each other. Aliases with
-- fewer dependencies will come first. This is useful if the generated code
-- requires earlier types to be earlier in the file.
sortAliases :: SymbolTable -> Array GenAlias -> Array GenAlias
sortAliases table arr = let
  tierList = SymbolTable.newTierList table
  in Array.sortWith (tier tierList) arr 
  where
  tier :: SymbolTable.AliasTierList -> GenAlias -> Int
  tier tierList (name /\ _) = let
    mtier = SymbolTable.tier name tierList
    in localEffect do 
      forceMaybe' ("Alias wasn't found in any tier: " <> show name) mtier
