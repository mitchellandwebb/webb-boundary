module Webb.Boundary.Analyzer.TypeSymbols where

import Prelude
import Webb.Boundary.Prelude
import Webb.Boundary.Data.Tree

import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.State (StateT, evalStateT, runStateT)
import Data.Array as Array
import Data.Either (Either)
import Effect.Aff (Aff, throwError)
import Effect.Aff.Class (liftAff)
import Webb.Boundary.Data.Alias (Alias)
import Webb.Boundary.Data.Alias as Alias
import Webb.Boundary.Data.SymbolTable (STable)
import Webb.Boundary.Data.SymbolTable as STable


{- Define the basic symbols in the global space, and their type representations. If we do _nominal_ type-checking, this is easy -- each type is just represented by a simple integer or string, and types are the same as long as they refer to the same value. There is no need, when doing type-checking, to do any structural comparisons. 

In our case, we do allow structural comparisons of record types, but boundary types are NOT allowed to be reused in concrete types.
-}

buildSymbolTable :: Tree -> Aff (Either (Array String) STable)
buildSymbolTable tree = do
  table <- newShowRef STable.emptyTable
  let 
    env = { tree, symbols: table } :: Env
    prog = do
      defined <- readDeclaredSymbols
      noDuplicates
      noRedefinedSymbols defined
      let combined = STable.union STable.predefined defined
      pure combined

  eval env prog

type Env = 
  { symbols :: ShowRef STable
  , tree :: Tree
  }
  
type Prog = ExceptT (Array String) (StateT Env Aff)
  
run :: Env -> Prog Unit -> Aff Unit
run env prog = void $ prog # runExceptT >>> flip runStateT env

eval :: forall a. Env -> Prog a -> Aff (Either (Array String) a)
eval env prog = prog # runExceptT >>> flip evalStateT env

-- Gets all the declared symbols from the tree. These have not been validated.
-- If a symbol appears twice, since there are no scopes at the top-level, we error.
-- But aliases are not yet checked for existence, since we don't have all symbols 
-- until after this function runs.
readDeclaredSymbols :: Prog STable
readDeclaredSymbols = do 
  this <- mread
  liftAff do 
    allAliases this.tree $ \alias -> run this $ declareAlias alias
  getSymbols
  
declareAlias :: Alias -> Prog Unit
declareAlias alias = do
  this <- mread
  STable.addAlias alias :> this.symbols

getSymbols :: Prog STable
getSymbols = do
  s <- mread
  aread s.symbols

noDuplicates :: Prog Unit
noDuplicates = do
  this <- mread
  dups <- STable.duplicates <: this.symbols
  let errors = dups <#> Alias.name >>> alreadyDefined
  tryThrow errors
  where
  alreadyDefined name = 
    "Global symbol has already been defined: " <> name 
  
-- The intersection of tables should be empty.
noRedefinedSymbols :: STable -> Prog Unit
noRedefinedSymbols new = do
  let 
    old = STable.predefined
    shared = STable.intersection old new
    symbols = Array.fromFoldable $ STable.symbols shared
    errors = symbols <#> \name -> illegal name
  tryThrow errors
  
  where 
  illegal name = "Illegally redefines an existing type: " <> name
  
tryThrow :: Array String -> Prog Unit
tryThrow errors = do
  unless (errors == []) do 
    throwError errors