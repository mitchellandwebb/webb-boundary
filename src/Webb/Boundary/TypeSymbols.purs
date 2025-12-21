module Webb.Boundary.TypeSymbols where

import Prelude
import Webb.Boundary.Prelude
import Webb.Boundary.Tree

import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Loops (anyM)
import Control.Monad.State (StateT, evalStateT, runStateT)
import Data.Array as A
import Data.Either (Either)
import Data.List as L
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Newtype (unwrap)
import Data.Tuple (uncurry)
import Effect.Aff (Aff, throwError)
import Effect.Aff.Class (liftAff)
import Webb.Boundary.Parser (AliasTarget(..), Param, TypeMap)
import Webb.Boundary.Parser as P
import Webb.Stateful (localEffect)
import Webb.Stateful.MapColl (MapColl, newMap)
import Webb.Stateful.MapColl as M


{- Define the basic symbols in the global space, and their type representations. If we do _nominal_ type-checking, this is easy -- each type is just represented by a simple integer or string, and types are the same as long as they refer to the same value. There is no need, when doing type-checking, to do any structural comparisons. 

In our case, we do allow structural comparisons of record types, but boundary types are NOT allowed to be reused in concrete types.
-}

data SymbolType 
  = INT
  | DOUBLE
  | BOOL
  | STRING
  | RECORD (Map String SymbolType)
  | PRODUCT Int
  | ALIAS Param 
  
type SymbolTable = Map String SymbolType

getGlobalSymbolTable :: Tree -> Aff (Either (Array String) SymbolTable)
getGlobalSymbolTable tree = do
  table <- newMap
  let 
    env = { tree, symbols: table } :: Env
    predefined = default
    prog = do
      defined <- readDeclaredSymbols
      noRedefinedSymbols predefined defined
      let combined = Map.union predefined defined
      pure combined

  eval env prog

default :: SymbolTable
default = Map.fromFoldable
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
  
type Env = 
  { symbols :: MapColl String SymbolType
  , tree :: Tree
  }
  
type Prog = ExceptT (Array String) (StateT Env Aff)
  
run :: Env -> Prog Unit -> Aff Unit
run env prog = void $ prog # runExceptT >>> flip runStateT env

eval :: forall a. Env -> Prog a -> Aff (Either (Array String) a)
eval env prog = prog # runExceptT >>> flip evalStateT env
  
declareAlias :: P.Alias -> Prog Unit
declareAlias alias = do
  this <- mread
  let name = alias.name.string
  whenM (M.member this.symbols name) do
    throwError [ alreadyDefined name ]

  case alias.target of
    AliasedParam wrapped -> do
      M.insert this.symbols name (ALIAS wrapped)
    AliasedMap m -> do
      M.insert this.symbols name (RECORD $ convert m)
      
  where
  convert :: TypeMap -> Map String SymbolType
  convert map = let 
    pairs = Map.toUnfoldable map :: Array _
    converted = pairs <#> uncurry \token wrapped -> 
      let name = token.string
      in name /\ ALIAS wrapped
    in Map.fromFoldable converted
  
  alreadyDefined name = 
    "Global symbol has already been defined: " <> name 

getSymbols :: Prog SymbolTable
getSymbols = do
  s <- mread
  aread s.symbols

-- Gets all the declared symbols from the tree. These have not been validated.
-- If a symbol appears twice, since there are no scopes at the top-level, we error.
-- But aliases are not yet checked for existence, since we don't have all symbols 
-- until after this function runs.
readDeclaredSymbols :: Prog SymbolTable
readDeclaredSymbols = do 
  this <- mread
  liftAff do 
    allAliases this.tree $ \alias -> run this $ declareAlias alias
  getSymbols
  
-- Check for redefinitions of old symbols in the new table, and publishes errors.
noRedefinedSymbols :: SymbolTable -> SymbolTable -> Prog Unit
noRedefinedSymbols old new = do
  let 
    shared = Map.intersection old new
    pairs = Map.toUnfoldable shared
    errors = pairs <#> uncurry \name _ -> illegal name
  when (errors == []) do
    throwError errors
  
  where 
  illegal name = "Illegally redefines an existing type: " <> name
    
typeExists :: String -> SymbolTable -> Boolean
typeExists name = Map.member name

isProduct :: String -> SymbolTable -> Boolean
isProduct name table = let 
  is = do
    entry <- Map.lookup name table
    case entry of
      PRODUCT _ -> pure true
      _ -> pure false
  in fromMaybe false is

-- Arg count of a symbol is either a result of it being a product, or 
-- the arg count of an aliased symbol, or the arg count 0.
argCount :: String -> SymbolTable -> Int
argCount name table = let 
  count = do
    entry <- Map.lookup name table
    case entry of
      PRODUCT i -> pure i
      ALIAS _ -> pure 0 -- even aliases have 0 args. They have no type arguments.
      _ -> pure 0
  in fromMaybe 0 count

-- Resolve the type symbol to a final _first_ type, converting all
-- aliases. CANNOT handle circular aliases that refer to themselves.
resolve :: String -> SymbolTable -> String
resolve name table = fromMaybe "unknown" do
  entry <- Map.lookup name table
  case entry of
    ALIAS wrapped -> do 
      -- An alias resolves to a different type symbol, across multiple aliases.
      let
        p = unwrap wrapped
        next = p.name.string
      pure $ resolve next table
    _ -> 
      -- Any other type resolves to its own symbol name.
      pure name
      
-- Check for a reference to the symbol within the parameter, or the parameters
-- arguments.
refersToSymbol :: String -> P.Param -> SymbolTable -> Boolean
refersToSymbol name wrapped table = 
  let
  param = unwrap wrapped
  pname = param.name.string
  args = param.args
  found = searchFor_ { symbol: name, startingFrom: pname }
  in if found then
    true
  else 
    A.any refersToMe args
  where
  searchFor_ args = searchFor args table
  refersToMe childParam = refersToSymbol name childParam table

-- Is the given symbol found somewhere?
searchFor :: 
  { symbol :: String, startingFrom :: String} -> SymbolTable -> Boolean
searchFor { symbol, startingFrom: other } table =
  if symbol == other then 
    true
  else 
    fromMaybe false do
      entry <- Map.lookup other table
      case entry of
        -- We have to search all params of the alias to find out if we are
        -- referring back to ourselves.
        ALIAS wrapped -> do 
          pure $ localEffect $ searchParam wrapped
        _ -> pure false
  where
  -- We search each of the params and child params, stopping when the first
  -- circular match is found.
  searchParam wrapped = do
    let 
      p = unwrap wrapped
      name = p.name.string
      args = p.args
      findSelf = pure $ searchFor { symbol, startingFrom: name } table
      findArgs = args <#> searchParam
      programs = [findSelf] <> findArgs
    anyM identity (L.fromFoldable programs)
      
    
    
    

    