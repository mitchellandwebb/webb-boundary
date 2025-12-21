module Webb.Boundary.BoundarySymbols where

import Webb.Boundary.Prelude
import Webb.Boundary.Tree

import Control.Monad.Except (ExceptT, lift, runExceptT, throwError)
import Control.Monad.State (StateT, evalStateT, runStateT)
import Data.Either (Either)
import Data.Foldable (for_)
import Data.Map (Map)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Webb.Boundary.Parser as P
import Webb.Boundary.Tokens (Token)
import Webb.Boundary.TypeCheck (methodParams, methodReturn)
import Webb.Stateful (localEffect)
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
  
type Concrete = P.Param

type Env = 
  { tree :: Tree
  , table :: MapColl String BoundaryEntry
  }
  
type Prog = ExceptT (Array String) (StateT Env Aff) 

run :: Env -> Prog Unit -> Aff Unit
run env prog = void $ prog # runExceptT >>> flip runStateT env

eval :: forall a. Env -> Prog a -> Aff (Either (Array String) a)
eval env prog = prog # runExceptT >>> flip evalStateT env

-- Get the table of all boundaries, and their methods.
getGlobalBoundaryTable :: Tree -> Aff (Either (Array String) BoundaryTable)
getGlobalBoundaryTable tree = do 
  table <- newMap
  let 
    env = { table, tree } :: Env
    prog = do
      -- There's only one source of boundaries, and no global ones. 
      -- So reading boundaries, and throwing on error, is enough.
      readBoundaries

  eval env prog

readBoundaries :: Prog BoundaryTable
readBoundaries = do
  this <- lift mread
  liftAff do
    allBoundaries this.tree \bound -> run this (addBoundary bound)
  getTable

addBoundary :: P.Boundary -> Prog Unit
addBoundary b = do 
  this <- lift mread
  let name = b.name.string
  
  functions <- getFunctionTable b.methods
  let entry = 
        { functions
        , name: b.name
        } :: BoundaryEntry
  
  whenM (M.member this.table name) do
    throwError $ [ "Boundary has already been defined: " <> name ]
    
  M.insert this.table name entry
  
  where 
  getFunctionTable :: Array P.Method -> Prog FunctionTable
  getFunctionTable methods = do
    fmap <- newMap
    for_ methods \method -> do
      let 
        name = method.name.string
        entry = 
          { name: method.name
          , params: localEffect $ methodParams method
          , return: localEffect $ methodReturn method
          } :: FunctionEntry
      
      whenM (M.member fmap name) do
        throwError [ "Function has already been defined: " <> name ]
      M.insert fmap name entry

    aread fmap
    
getTable :: Prog BoundaryTable
getTable = do
  this <- lift mread
  lift $ aread this.table
  