module Webb.Boundary.BoundarySymbols where

import Webb.Boundary.Prelude
import Webb.Boundary.Tree

import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.State (StateT, evalStateT, runStateT)
import Data.Array as Array
import Data.Either (Either)
import Data.Set as Set
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Webb.Boundary.Data.Boundary (Boundary)
import Webb.Boundary.Data.Boundary as Bd
import Webb.Boundary.Data.BoundaryTable (BTable)
import Webb.Boundary.Data.BoundaryTable as BTable
import Webb.Boundary.Data.Method as Method
import Webb.Boundary.Data.SymbolTable (STable)
import Webb.Boundary.Data.SymbolTable as STable


{- Read the boundary symbols out. These are not themselves types, and cannot be reused, but _do_ themselves have names that must be unique, and do define
functions that themselves have type signatures that do contain types -- types that
will need to be compared to the TypeSymbols.
-}

type Env = 
  { tree :: Tree
  , symbols :: STable
  , table :: ShowRef BTable
  }
  
type Prog = ExceptT (Array String) (StateT Env Aff) 

run :: Env -> Prog Unit -> Aff Unit
run env prog = void $ prog # runExceptT >>> flip runStateT env

eval :: forall a. Env -> Prog a -> Aff (Either (Array String) a)
eval env prog = prog # runExceptT >>> flip evalStateT env

-- Get the table of all boundaries, and their methods.
getGlobalBoundaryTable :: 
  Tree -> STable -> Aff (Either (Array String) BTable)
getGlobalBoundaryTable tree symbols = do 
  table <- newShowRef BTable.emptyTable
  let 
    env = { table, tree, symbols } :: Env
    prog = do
      btable <- readBoundaries
      noDuplicates  
      noSymbolClash
      pure btable

  eval env prog

readBoundaries :: Prog BTable
readBoundaries = do
  this <- mread
  liftAff do
    allBoundaries this.tree \bound -> run this (addBoundary bound)
  aread this.table

addBoundary :: Boundary -> Prog Unit
addBoundary b = do 
  this <- mread
  BTable.insert b :> this.table
  
noDuplicates :: Prog Unit
noDuplicates = do
  this <- mread
  dupBounds <- BTable.duplicates <: this.table
  let boundErrors = (alreadyDefined <<< Bd.name) <$> dupBounds
  tryThrow boundErrors
    
  dupMethods <- BTable.duplicateMethods <: this.table
  let methodErrors = (alreadyDefined <<< Method.name) <$> dupMethods
  tryThrow methodErrors
    
  where
  alreadyDefined name = "Symbol has already been defined: " <> name 
     
noSymbolClash :: Prog Unit
noSymbolClash = do
  this <- mread
  boundaries <- BTable.boundaries <: this.table
  let
    bnames = Set.fromFoldable $ Bd.name <$> boundaries
    snames = Set.fromFoldable $ STable.symbols this.symbols
    shared = Array.fromFoldable $ Set.intersection bnames snames
    errors = shared <#> alreadyDefined
    
  tryThrow errors

  where
  alreadyDefined name = "Symbol has already been defined: " <> name 
  
tryThrow :: Array String -> Prog Unit
tryThrow errors = do
  unless (errors == []) do
    throwError errors
  
    
  