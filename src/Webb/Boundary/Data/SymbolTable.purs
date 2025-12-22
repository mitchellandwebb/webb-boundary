module Webb.Boundary.Data.SymbolTable where

import Prelude

import Control.Monad.Loops (anyM)
import Data.Array as Array
import Data.Foldable (maximum)
import Data.Foldable as Fold
import Data.Generic.Rep (class Generic)
import Data.Lens (Lens', over)
import Data.Lens.Record (prop)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Set (Set)
import Data.Set as Set
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple, uncurry)
import Data.Tuple.Nested ((/\), type (/\))
import Effect (Effect)
import Type.Proxy (Proxy(..))
import Webb.Boundary.Data.Alias (Alias)
import Webb.Boundary.Data.Alias as Alias
import Webb.Boundary.Data.Param (Param)
import Webb.Boundary.Data.Param as Param
import Webb.Boundary.Data.TypeMap as TypeMap
import Webb.Boundary.Data.Utils (findDuplicatesWith)
import Webb.Monad.Prelude (forceMaybe')
import Webb.State.Prelude ((<:))
import Webb.Stateful (localEffect)
import Webb.Stateful.MapColl as MapColl


newtype SymbolTable = SymbolTable_ SymbolTable_
type STable = SymbolTable

type SymbolTable_ = 
  { map :: Map String SymbolType
  , array :: Array Alias
  }

_map :: forall a r. Lens' { map :: a | r } a
_map = prop (Proxy :: Proxy "map")

_array :: forall a r. Lens' { array :: a | r } a
_array = prop (Proxy :: Proxy "array")

derive newtype instance Eq SymbolTable
derive newtype instance Ord SymbolTable
derive instance Newtype SymbolTable _

emptyTable :: SymbolTable
emptyTable = wrap { map: Map.empty, array: [] }

predefined :: SymbolTable
predefined = wrap 
  { map: Map.fromFoldable pairs'
  , array: []
  }
  where 
  pairs' = 
    [ "Int" /\ INT
    , "Number" /\ DOUBLE
    , "Double" /\ DOUBLE
    , "String" /\ STRING

    , "Array" /\ PRODUCT 1
    , "Map" /\ PRODUCT 2
    , "Tuple" /\ PRODUCT 2
    , "Record" /\ PRODUCT 1
    
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
  | PRODUCT Int
  | ALIAS Alias
  
derive instance Eq SymbolType
derive instance Ord SymbolType
derive instance Generic SymbolType _
instance Show SymbolType where 
  show = case _ of
    x -> genericShow x
    
_addEntry :: String -> SymbolType -> SymbolTable -> SymbolTable
_addEntry sym stype table = 
  table # unwrap >>> 
    over _map (Map.insert sym stype) >>> 
    wrap

_insert :: String -> SymbolType -> SymbolTable -> SymbolTable
_insert = _addEntry

addAlias :: Alias -> SymbolTable -> SymbolTable
addAlias alias table = let 
  t2 = (table # unwrap >>> 
          over _array (flip Array.snoc alias) >>>
          wrap) :: SymbolTable
  in _insert (Alias.name alias) (ALIAS alias) t2

member :: String -> SymbolTable -> Boolean
member sym = unwrap >>> _.map >>> Map.member sym

lookup :: String -> SymbolTable -> Maybe SymbolType
lookup sym = unwrap >>> _.map >>> Map.lookup sym

declarations :: SymbolTable -> Array Alias
declarations = unwrap >>> _.array

aliases :: SymbolTable -> Array Alias
aliases table = let 
  types' = types table
  maliases = types' <#> case _ of 
    ALIAS alias -> Just alias
    _ -> Nothing
  in Array.catMaybes maliases

symbols :: SymbolTable -> Set String
symbols = unwrap >>> _.map >>> Map.keys

pairs :: SymbolTable -> Array (Tuple String SymbolType)
pairs = unwrap >>> _.map >>> Map.toUnfoldable

types :: SymbolTable -> Array SymbolType
types = unwrap >>> _.map >>> Map.values >>> Array.fromFoldable

intersection :: SymbolTable -> SymbolTable -> SymbolTable
intersection a b = let
  a' = unwrap a
  b' = unwrap b
  newArray = a'.array <> b'.array
  newMap = Map.intersection a'.map b'.map
  in wrap { map: newMap, array: newArray }

union :: SymbolTable -> SymbolTable -> SymbolTable
union a b = let
  a' = unwrap a
  b' = unwrap b
  newArray = a'.array <> b'.array
  newMap = Map.union a'.map b'.map
  in wrap { map: newMap, array: newArray }

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
    ALIAS alias -> do 
      case Alias.target alias of
        Alias.AliasedParam param -> do
          -- An alias resolves to a final higher type, across multiple aliases.
          resolveParam param
        Alias.AliasedMap _ -> do
          -- An aliased map is just a record
          pure "Record"
          
    _x -> do
      -- Any other type is already the highest type it can be.
      pure name
    where
    resolveParam param = do
      let next = Param.name param
      resolveToHigherType next table
      
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
        ALIAS alias -> do 
          case Alias.target alias of
            Alias.AliasedParam param -> do
              pure $ localEffect $ checkParam param
            Alias.AliasedMap map -> do
              let params = TypeMap.params map
                  programs = params <#> checkParam :: Array (Effect Boolean)
                  anyMatch = anyM identity (List.fromFoldable programs)
              pure $ localEffect anyMatch
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
    ALIAS al -> 
      case Alias.target al of
        Alias.AliasedParam param -> 
          _isRelatedParam param 
        Alias.AliasedMap map -> let
          params = TypeMap.params map
          in
          Array.any _isRelatedParam params
    _ -> false

  _isRelatedParam param = let 
    syms = Param.symbols param
    in Array.any _isRelated syms
    
-- Check for any duplicates in the alias declaration.
duplicates :: SymbolTable -> Array Alias
duplicates table = let 
  decls = declarations table
  in findDuplicatesWith Alias.name decls
    

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
    aliases' = Array.catMaybes maliases :: Array String
    
  Fold.for_ aliases' \name -> do
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
      ALIAS alias -> do
        case Alias.target alias of
          Alias.AliasedParam param -> do
            getParamScore score param 
          Alias.AliasedMap map -> do
            let params = TypeMap.params map
                scores = Array.catMaybes $ params <#> getParamScore score
            maximum scores
      _ -> do
        pure score

  getParamScore :: Int -> Param -> Maybe Int
  getParamScore score param = let 
    score' = score + 1 :: Int
    pname = Param.name param
    nameScore = rootLength pname score' :: Int
    argNames = Param.args param <#> Param.name :: Array String
    argScores = (argNames <#> \argName -> rootLength argName score') :: Array Int
    allScores = [ nameScore ] <> argScores :: Array Int
    in
    -- How far param is from a root node.
    maximum allScores

tier :: String -> AliasTierList -> Maybe Int
tier alias list = let
  array = unwrap list
  in do 
    i /\ _ <- Array.find isMember array
    pure i
  where
  isMember (_ /\ set) = Set.member alias set
