module Webb.Boundary.Gen where

import Webb.Boundary.Prelude
import Webb.Boundary.Tree

import Data.Array as Array
import Data.Either (Either)
import Data.Foldable (for_, maximum)
import Data.Foldable as Trav
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (fst, snd)
import Effect.Aff (Aff, throwError)
import Webb.Boundary.BoundarySymbols (BoundaryTable)
import Webb.Boundary.Data.Token as Token
import Webb.Boundary.Parser as P
import Webb.Boundary.TypeCheck (methodParams, methodReturn)
import Webb.Boundary.TypeSymbols (SymbolTable, SymbolType(..))
import Webb.Boundary.Data.Param (Param)
import Webb.Boundary.Data.Param as Param
import Webb.Monad.Prelude (forceMaybe')
import Webb.Stateful (localEffect)
import Webb.Stateful.ArrayColl (newArray)
import Webb.Stateful.ArrayColl as Arr
import Webb.Stateful.MapColl as MC


{- Define the data structures and functions that will be used by generators to build code.
-}

type GenEnv = 
  { symbols :: SymbolTable
  , boundaries :: BoundaryTable
  , writable ::
    { aliases :: Array Alias
    , boundaries :: Array Boundary
    }
  }
  
-- Either we are ready to generate, or we got errors
buildEnv :: String -> Aff (Either (Array String) GenEnv)
buildEnv file = do
  -- TODO -- we parse the file, perform checks on it, and then get the writable
  -- aliases and boundaries
  pure $ throwError []


type Alias = String /\ AliasTarget

data AliasTarget = AliasMap TypeMap | AliasParam Param

type Boundary = String /\ Array FnDef

type TypeMap = Map String Param

type FnDef = 
  { name :: String
  , args :: Array Param
  , return :: Param
  }
  
getAliases :: Tree -> Aff (Array Alias)
getAliases tree = do
  arr <- newArray
  allAliases tree \alias -> addAlias arr alias
  aread arr
  
  where
  addAlias arr alias = do
    let name = Token.text alias.name
    case alias.target of
      P.AliasedParam p -> do
        Arr.addLast arr $ name /\ AliasParam p
      P.AliasedMap m -> do
        Arr.addLast arr $ name /\ AliasMap (convert m)
        
  convert map = let
    pairs = Map.toUnfoldable map :: Array _
    converted = pairs <#> uncurry \token param -> Token.text token /\ param
    in Map.fromFoldable converted

getBoundaries :: Tree -> Aff (Array Boundary)
getBoundaries tree = do
  boundArray <- newArray
  allBoundaries tree \b -> addBound boundArray b
  aread boundArray
  
  where
  addBound boundArray b = do
    let name = Token.text b.name
    defArray <- newArray
    for_ b.methods \m -> addDef defArray m
    
    defs <- aread defArray
    Arr.addLast boundArray $ name /\ defs
    
  addDef defArray m = do
    let name = Token.text m.name
    args <- methodParams m
    return <- methodReturn m
    Arr.addLast defArray { name, args, return }

-- Sort the aliases in order of their dependency on each other. Aliases with
-- fewer dependencies will come first. This is useful if the generated code
-- requires earlier types to be earlier in the file.
sortAliases :: SymbolTable -> Array Alias -> Array Alias
sortAliases table arr = let
  tierList = aliasTierList table
  in Array.sortWith (tier tierList) arr 
  where
  tier :: AliasTierList -> Alias -> Int
  tier tierList (name /\ _) = let
    mindex = Array.findIndex (Set.member name) tierList
    in localEffect do 
      forceMaybe' ("Alias wasn't found in any tier: " <> show name) mindex

type AliasTierList = Array (Set String)

-- Use the symbol table to define a tier list of alias symbols, by finding the longest
-- path back to the top of the tree for each symbol.
aliasTierList :: SymbolTable -> AliasTierList
aliasTierList table = localEffect do 
  tiers <- MC.newMap
  let 
    pairs = Map.toUnfoldable table :: Array _
    maliases = pairs <#> uncurry \name val -> case val of
      ALIAS _ -> Just name
      _ -> Nothing
    aliases = Array.catMaybes maliases :: Array String
    
  Trav.for_ aliases \name -> do
    let score = rootLength name 0
    MC.update tiers score (Set.singleton name) (Set.insert name)
  
  -- Once we have all tier-sets, sort them by scores, and  publish the tiers
  -- in order of the scores.
  entries :: Array _ <- Map.toUnfoldable <: tiers
  let final = entries # Array.sortWith fst >>> map snd :: Array (Set String)
  pure final
  
  where
  -- Measure the length of the symbol from the root.
  rootLength :: String -> Int -> Int
  rootLength name score = localEffect do 
    let mscore = getScore name score
    forceMaybe' ("Found no score for: " <> name) mscore

  getScore :: String -> Int -> Maybe Int
  getScore name score = do 
    entry <- Map.lookup name table
    case entry of 
      ALIAS param -> do
        let 
          score' = score + 1 :: Int
          pname = Param.name param
          nameScore = rootLength pname score' :: Int
          argNames = Param.args param <#> Param.name :: Array String
          argScores = (argNames <#> \argName -> rootLength argName score') :: Array Int
          allScores = [ nameScore ] <> argScores :: Array Int

        -- The longest length is how far the param is from the root in _any_ of
        -- its symbols.
        maximum allScores
      _ -> do 
        pure score  -- Anything else terminates the search; we reached the root.

