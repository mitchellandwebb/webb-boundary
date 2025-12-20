module Webb.Boundary.BoundarySymbols where

import Prelude
import Webb.Boundary.Prelude
import Webb.Boundary.Tree

import Data.Array as A
import Data.Either (Either)
import Data.Foldable (for_)
import Data.Map (Map)
import Effect (Effect)
import Effect.Aff (Aff)
import Webb.Boundary.Parser as P
import Webb.Boundary.Tokens (Token)
import Webb.Monad.Prelude (forceMaybe', throwString)
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

-- Get the table of all boundaries, and their methods.
getGlobalBoundaryTable :: Tree -> Aff (Either (Array String) BoundaryTable)
getGlobalBoundaryTable tree = do 
  table <- readBoundaries tree
  
  -- There are no other boundaries defined elsewhere. So no need to compare them
  -- to anything besides what is in this file.
  pure $ pure table

readBoundaries :: Tree -> Aff BoundaryTable
readBoundaries tree = do
  v <- newBoundaryVisitor
  visit v tree
  getTable v
  
newtype BoundaryVisitor = BV (MapColl String BoundaryEntry)

instance Visitor BoundaryVisitor where
  boundary (BV map) b = do 
    let 
      name = b.name.string
      entry = 
        { functions: localEffect do getFunctionTable b.methods
        , name: b.name
        } :: BoundaryEntry
    
    whenM (M.member map name) do
      throwString $ "Boundary has already been defined: " <> name
      
    M.insert map name entry
    
    where 
    getFunctionTable :: Array P.Method -> Effect FunctionTable
    getFunctionTable methods = do
      fmap <- newMap
      for_ methods \method -> do
        let 
          name = method.name.string
          entry = 
            { name: method.name
            , params: localEffect $ getParams method
            , return: localEffect $ getReturn method
            } :: FunctionEntry
        
        whenM (M.member fmap name) do
          throwString $ "Function has already been defined: " <> name
        M.insert fmap name entry

      aread fmap
      
    getParams :: P.Method -> Effect (Array P.Param)
    getParams method = do
      let minit = A.init method.params
      forceMaybe' "No function parameters were provided" minit
      
    getReturn :: P.Method -> Effect P.Param
    getReturn method = do
      let mlast = A.last method.params
      forceMaybe' "No function parameters were provided" mlast

  method = defaultMethod
  param = defaultParam
  alias = defaultAlias
  typeMap = defaultTypeMap

newBoundaryVisitor :: Aff BoundaryVisitor
newBoundaryVisitor = do
  map <- newMap
  pure $ BV map
  
getTable :: BoundaryVisitor -> Aff BoundaryTable
getTable (BV s) = aread s
  