module Webb.Boundary.Data.SymbolTable where

import Prelude

import Control.Monad.Loops (anyM)
import Data.Array as Array
import Data.Foldable (maximum)
import Data.Foldable as Fold
import Data.Generic.Rep (class Generic)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Set (Set)
import Data.Set as Set
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple, fst, snd, uncurry)
import Data.Tuple.Nested ((/\), type (/\))
import Webb.Boundary.Data.Param (Param)
import Webb.Boundary.Data.Param as Param
import Webb.Monad.Prelude (forceMaybe')
import Webb.State.Prelude ((<:))
import Webb.Stateful (localEffect)
import Webb.Stateful.MapColl as MapColl


newtype SymbolTable = SymbolTable_ SymbolTable_

type SymbolTable_ = Map String SymbolType

derive newtype instance Eq SymbolTable
derive newtype instance Ord SymbolTable
derive instance Newtype SymbolTable _

emptyTable :: SymbolTable
emptyTable = wrap Map.empty

predefined :: SymbolTable
predefined = wrap $ Map.fromFoldable
  [ "Int" /\ INT
  , "Number" /\ DOUBLE
  , "Double" /\ DOUBLE
  , "String" /\ STRING

  , "Array" /\ PRODUCT 1
  , "Map" /\ PRODUCT 2
  , "Tuple" /\ PRODUCT 2
  
  -- The complex function types must be the _return_ types of a function, but
  -- can appear in an alias. There is no function type otherwise. The complex function
  -- types CANNOT be arguments.
  , "Effect" /\ PRODUCT 1
  , "Aff" /\ PRODUCT 1
  ]

data SymbolType 
  = INT
  | DOUBLE
  | BOOL
  | STRING
  | RECORD (Map String SymbolType)
  | PRODUCT Int
  | ALIAS Param
  
derive instance Eq SymbolType
derive instance Ord SymbolType
derive instance Generic SymbolType _
instance Show SymbolType where 
  show = case _ of
    RECORD map -> show map
    x -> genericShow x
    
addEntry :: String -> SymbolType -> SymbolTable -> SymbolTable
addEntry sym stype table = table # unwrap >>> Map.insert sym stype >>> wrap

insert :: String -> SymbolType -> SymbolTable -> SymbolTable
insert = addEntry

member :: String -> SymbolTable -> Boolean
member sym = unwrap >>> Map.member sym

lookup :: String -> SymbolTable -> Maybe SymbolType
lookup sym = unwrap >>> Map.lookup sym

symbols :: SymbolTable -> Set String
symbols = unwrap >>> Map.keys

pairs :: SymbolTable -> Array (Tuple String SymbolType)
pairs = unwrap >>> Map.toUnfoldable

types :: SymbolTable -> Array SymbolType
types = unwrap >>> Map.values >>> Array.fromFoldable

intersection :: SymbolTable -> SymbolTable -> SymbolTable
intersection a b = let
  a' = unwrap a
  b' = unwrap b
  in wrap $ Map.intersection a' b'

union :: SymbolTable -> SymbolTable -> SymbolTable
union a b = let
  a' = unwrap a
  b' = unwrap b
  in wrap $ Map.union a' b'

argCount :: String -> SymbolTable -> Int
argCount sym table = fromMaybe 0 do
  t <- lookup sym table
  case t of
    PRODUCT i -> pure i
    _ -> pure 0

isProduct :: String -> SymbolTable -> Boolean
isProduct sym table = fromMaybe false do
  t <- lookup sym table
  case t of
    PRODUCT _ -> pure true
    _ -> pure false
    
-- Resolves the symbol to a higher type, bypassing aliases
resolveToHigherType :: String -> SymbolTable -> Maybe String
resolveToHigherType name table = do
  entry <- lookup name table
  case entry of
    ALIAS param -> do 
      -- An alias resolves to a final higher type, across multiple aliases.
      let next = Param.name param
      resolveToHigherType next table
    _x -> do
      -- Any other type is already the highest type it can be.
      pure name
      
-- Determine if a symbol is related to another symbol, traveling through all
-- names in any alias parameters if necessary.
isRelated :: 
  { symbol :: String, ancestor :: String} -> SymbolTable -> Boolean
isRelated { symbol, ancestor } table =
  if symbol == ancestor then 
    true
  else 
    fromMaybe false do
      entry <- lookup ancestor table
      case entry of
        -- We have to search all params of the alias to find out if we are
        -- referring back to ourselves.
        ALIAS param -> do 
          pure $ localEffect $ checkParam param
        _ -> pure false
  where
  -- We search each of the params and child params, stopping when the first
  -- circular match is found.
  checkParam param = do
    let 
      name = Param.name param
      args = Param.args param
      relatedToParam = pure $ isRelated { symbol, ancestor: name } table
      relatedToArgs = args <#> checkParam
      programs = [relatedToParam] <> relatedToArgs
      
    -- We use 'any' to avoid executing all of them, so we terminate.
    anyM identity (List.fromFoldable programs)
      
-- Check for a reference to the symbol within the parameter, or the parameter's
-- arguments.
isCircularAlias :: String -> SymbolTable -> Boolean
isCircularAlias alias table = fromMaybe false do
  entry <- lookup alias table
  pure $ _isRelatedType entry
  where
  _isRelated symbol = isRelated { symbol, ancestor: alias } table
  
  _isRelatedType symType = case symType of
    ALIAS param ->
      let syms = Param.symbols param
      in Array.any _isRelated syms
      
    -- If the alias points to a record, look through the params.
    RECORD map -> 
      let symbolTypes = Array.fromFoldable $ Map.values map
      in  Array.any _isRelatedType symbolTypes
    _ -> false
    

-- Use the symbol table to define a tier list of alias symbols, by finding the longest
-- path back to the top of the tree for each symbol.
newtype AliasTierList = Atl (Array (Int /\ (Set String)))
derive instance Newtype AliasTierList _
derive newtype instance Eq AliasTierList
derive newtype instance Ord AliasTierList
derive newtype instance Show AliasTierList

newTierList :: SymbolTable -> AliasTierList
newTierList table = localEffect do 
  tiers <- MapColl.newMap
  let 
    pairs' = pairs table :: Array _
    maliases = pairs' <#> uncurry \name val -> case val of
      ALIAS _ -> Just name
      _ -> Nothing
    aliases = Array.catMaybes maliases :: Array String
    
  Fold.for_ aliases \name -> do
    let score = rootLength name 0
    MapColl.update tiers score (Set.singleton name) (Set.insert name)
  
  -- Once we have all tier-sets, sort them by scores, and  publish the tiers
  -- in order of the scores.
  entries :: Array _ <- Map.toUnfoldable <: tiers
  pure $ wrap entries
  
  where
  -- Measure the length of the symbol from the root.
  rootLength :: String -> Int -> Int
  rootLength name score = localEffect do 
    let mscore = getScore name score
    forceMaybe' ("Found no score for: " <> name) mscore

  getScore :: String -> Int -> Maybe Int
  getScore name score = do 
    entry <- lookup name table
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

tier :: String -> AliasTierList -> Maybe Int
tier alias list = let
  array = unwrap list
  in do 
    i /\ _ <- Array.find isMember array
    pure i
  where
  isMember (_ /\ set) = Set.member alias set
