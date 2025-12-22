module Webb.Boundary.TypeSymbols where

import Prelude
import Webb.Boundary.Prelude
import Webb.Boundary.Tree

import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.State (StateT, evalStateT, runStateT)
import Data.Array as Array
import Data.Either (Either)
import Data.Map (Map)
import Data.Map as Map
import Effect.Aff (Aff, throwError)
import Effect.Aff.Class (liftAff)
import Webb.Boundary.Data.Alias (Alias)
import Webb.Boundary.Data.Alias as Alias
import Webb.Boundary.Data.SymbolTable (SymbolTable, SymbolType(..))
import Webb.Boundary.Data.SymbolTable as SymbolTable
import Webb.Boundary.Data.Token as Token
import Webb.Boundary.Data.TypeMap (TypeMap)
import Webb.Boundary.Data.TypeMap as TypeMap


{- Define the basic symbols in the global space, and their type representations. If we do _nominal_ type-checking, this is easy -- each type is just represented by a simple integer or string, and types are the same as long as they refer to the same value. There is no need, when doing type-checking, to do any structural comparisons. 

In our case, we do allow structural comparisons of record types, but boundary types are NOT allowed to be reused in concrete types.
-}

getGlobalSymbolTable :: Tree -> Aff (Either (Array String) SymbolTable)
getGlobalSymbolTable tree = do
  table <- newShowRef SymbolTable.emptyTable
  let 
    env = { tree, symbols: table } :: Env
    predefined = SymbolTable.predefined
    prog = do
      defined <- readDeclaredSymbols
      noRedefinedSymbols predefined defined
      let combined = SymbolTable.union predefined defined
      pure combined

  eval env prog

type Env = 
  { symbols :: ShowRef SymbolTable
  , tree :: Tree
  }
  
type Prog = ExceptT (Array String) (StateT Env Aff)
  
run :: Env -> Prog Unit -> Aff Unit
run env prog = void $ prog # runExceptT >>> flip runStateT env

eval :: forall a. Env -> Prog a -> Aff (Either (Array String) a)
eval env prog = prog # runExceptT >>> flip evalStateT env
  
declareAlias :: Alias -> Prog Unit
declareAlias alias = do
  this <- mread
  let name = Alias.name alias
  whenM (SymbolTable.member name <: this.symbols) do
    throwError [ alreadyDefined name ]

  case Alias.target alias of
    Alias.AliasedParam wrapped -> do
      SymbolTable.insert name (ALIAS wrapped) :> this.symbols
    Alias.AliasedMap m -> do
      SymbolTable.insert name (RECORD $ convert m) :> this.symbols
      
  where
  convert :: TypeMap -> Map String SymbolType
  convert map = let 
    pairs = TypeMap.pairs map
    converted = pairs <#> uncurry \token param -> 
      let name = Token.text token
      in name /\ ALIAS param
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
    shared = SymbolTable.intersection old new
    symbols = Array.fromFoldable $ SymbolTable.symbols shared
    errors = symbols <#> \name -> illegal name
  when (errors == []) do
    throwError errors
  
  where 
  illegal name = "Illegally redefines an existing type: " <> name