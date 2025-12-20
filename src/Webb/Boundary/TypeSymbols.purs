module Webb.Boundary.TypeSymbols where

import Prelude
import Webb.Boundary.Prelude
import Webb.Boundary.Tree

import Data.Array as A
import Data.Either (Either)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.Tuple (uncurry)
import Effect.Aff (Aff, throwError)
import Webb.Boundary.Parser (AliasTarget(..), TypeMap)
import Webb.Monad.Prelude (throwString)
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
  | ALIAS String -- We are typed by aliasing another symbol.
  
type SymbolTable = Map String SymbolType

getGlobalSymbolTable :: Tree -> Aff (Either (Array String) SymbolTable)
getGlobalSymbolTable tree = do
  let predefined = default
  defined <- readDeclaredSymbols tree
  let 
    either = do
      noRedefinedSymbols predefined defined
      let combined = Map.union predefined defined
      noDanglingAliases combined
      pure combined
  pure either

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
  
newtype SymbolVisitor = SV 
  { symbols :: MapColl String SymbolType

  }
  
instance Visitor SymbolVisitor where
  alias (SV v) node = do
    let name = node.name.string
    whenM (M.member v.symbols name) do
      throwString $ "Global symbol has already been defined: " <> name

    case node.target of
      AliasedParam wrapped -> do
        let 
          p = unwrap wrapped
          pname = p.name.string
        M.insert v.symbols name (ALIAS pname)
      AliasedMap m -> do
        M.insert v.symbols name (RECORD $ convert m)
        
    where
    convert :: TypeMap -> Map String SymbolType
    convert map = let 
      pairs = Map.toUnfoldable map :: Array _
      converted = pairs <#> uncurry \token wrapped -> 
        let
        param = unwrap wrapped
        name = token.string
        pname = param.name.string
        in name /\ ALIAS pname
      in Map.fromFoldable converted
        
  boundary = defaultBoundary
  method = defaultMethod
  param = defaultParam
  typeMap = defaultTypeMap

newSymbolVisitor :: Aff SymbolVisitor
newSymbolVisitor = do
  syms <- newMap
  pure $ SV { symbols: syms }
  
getSymbols :: SymbolVisitor -> Aff SymbolTable
getSymbols (SV s) = do
  aread s.symbols

-- Gets all the declared symbols from the tree. These have not been validated.
-- If a symbol appears twice, since there are no scopes at the top-level, we error.
-- But aliases are not yet checked for existence, since we don't have all symbols 
-- until after this function runs.
readDeclaredSymbols :: Tree -> Aff SymbolTable
readDeclaredSymbols tree = do 
  v <- newSymbolVisitor
  visit v tree 
  getSymbols v
  
-- Check for redefinitions of old symbols in the new table, and publishes errors.
noRedefinedSymbols :: SymbolTable -> SymbolTable -> Either (Array String) Unit
noRedefinedSymbols old new = 
  let 
  shared = Map.intersection old new
  pairs = Map.toUnfoldable shared
  errors = pairs <#> uncurry \name _ ->  
    "Illegally redefines an existing type: " <> name
  in 
  if errors == [] then do
    throwError errors
  else do 
    pure unit
    
-- Checks that all aliases refer to existing symbols in the table
noDanglingAliases :: SymbolTable -> Either (Array String) Unit
noDanglingAliases symbols = 
  let
  pairs = Map.toUnfoldable symbols
  errors = A.catMaybes $ pairs <#> uncurry \name entry -> 
    case entry of 
      ALIAS target -> 
        if Map.member target symbols then 
          Nothing
        else 
          Just $ "Alias " <> name <> " refers to undefined symbol " <> target
      _ -> Nothing
  in 
  if errors == [] then do
    throwError errors
  else do
    pure unit

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

argCount :: String -> SymbolTable -> Int
argCount name table = let 
  count = do
    entry <- Map.lookup name table
    case entry of
      PRODUCT i -> pure i
      _ -> pure 0
  in fromMaybe 0 count

-- Resolve the type symbol to another a final type, ignoring all
-- aliases
resolve :: String -> SymbolTable -> String
resolve name table = fromMaybe "unknown" do
  entry <- Map.lookup name table
  case entry of
    ALIAS next -> 
      -- An alias resolves to a different type symbol, across multiple aliases.
      pure $ resolve next table
    _ -> 
      -- Any other type resolves to its own symbol name.
      pure name
    