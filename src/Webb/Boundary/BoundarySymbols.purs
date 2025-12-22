module Webb.Boundary.BoundarySymbols where

import Webb.Boundary.Prelude
import Webb.Boundary.Tree

import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.State (StateT, evalStateT, runStateT)
import Data.Either (Either)
import Data.Foldable (for_)
import Data.Map (Map)
import Data.Map as Map
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Webb.Boundary.Data.Boundary (Boundary)
import Webb.Boundary.Data.Boundary as Bound
import Webb.Boundary.Data.Method (Method)
import Webb.Boundary.Data.Method as Method
import Webb.Boundary.Data.Param (Param)
import Webb.Boundary.Data.Token (Token)
import Webb.Boundary.TypeSymbols (SymbolTable)
import Webb.Stateful.MapColl (MapColl, newMap)
import Webb.Stateful.MapColl as M


{- Read the boundary symbols out. These are not themselves types, and cannot be reused, but _do_ themselves have names that must be unique, and do define
functions that themselves have type signatures that do contain types -- types that
will need to be compared to the TypeSymbols.
-}

type BoundaryTable = Map String BoundaryEntry

type BoundaryEntry = 
  { functions :: FunctionTable
  , name :: Token
  }
  
type FunctionTable = Map String FunctionEntry

type FunctionEntry = 
  { name :: Token
  , params :: Array Concrete
  , return :: Concrete
  }
  
type Concrete = Param

type Env = 
  { tree :: Tree
  , symbols :: SymbolTable
  , table :: MapColl String BoundaryEntry
  }
  
type Prog = ExceptT (Array String) (StateT Env Aff) 

run :: Env -> Prog Unit -> Aff Unit
run env prog = void $ prog # runExceptT >>> flip runStateT env

eval :: forall a. Env -> Prog a -> Aff (Either (Array String) a)
eval env prog = prog # runExceptT >>> flip evalStateT env

-- Get the table of all boundaries, and their methods.
getGlobalBoundaryTable :: 
  Tree -> SymbolTable -> Aff (Either (Array String) BoundaryTable)
getGlobalBoundaryTable tree symbols = do 
  table <- newMap
  let 
    env = { table, tree, symbols } :: Env
    prog = do
      -- There's only one source of boundaries, and no global ones. 
      -- So reading boundaries, and throwing on error, is enough.
      readBoundaries

  eval env prog

readBoundaries :: Prog BoundaryTable
readBoundaries = do
  this <- mread
  liftAff do
    allBoundaries this.tree \bound -> run this (addBoundary bound)
  getTable

addBoundary :: Boundary -> Prog Unit
addBoundary b = do 
  this <- mread
  let name = Bound.name b
  
  functions <- getFunctionTable $ Bound.methods b
  let entry = 
        { functions
        , name: Bound.nameToken b
        } :: BoundaryEntry
  
  -- Cannot clash with an existing boundary.
  whenM (M.member this.table name) do
    throwError $ [ alreadyDefined name ]
    
  -- Cannot clash with an existing type.
  when (Map.member name this.symbols) do 
    throwError $ [ alreadyDefined name ]
    
  M.insert this.table name entry
  
  where 
  getFunctionTable :: Array Method -> Prog FunctionTable
  getFunctionTable methods = do
    fmap <- newMap
    for_ methods \method -> do
      let 
        name = Method.name method
        entry = 
          { name: Method.nameToken method
          , params: Method.firstParams method
          , return: Method.returnParam method
          } :: FunctionEntry
      
      whenM (M.member fmap $ name) do
        throwError [ "Function has already been defined: " <> name ]
      M.insert fmap name entry

    aread fmap
    
  alreadyDefined name = 
     "Boundary has already been defined: " <> name 
    
getTable :: Prog BoundaryTable
getTable = do
  this <- mread
  aread this.table
  