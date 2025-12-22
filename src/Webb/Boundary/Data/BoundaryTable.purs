module Webb.Boundary.Data.BoundaryTable where

import Prelude

import Data.Array as Array
import Data.Lens (Lens', view)
import Data.Lens.Record (prop)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe, isJust)
import Data.Newtype (class Newtype, unwrap, wrap)
import Type.Proxy (Proxy(..))
import Webb.Boundary.Data.Boundary (Boundary)
import Webb.Boundary.Data.Boundary as Bound
import Webb.Boundary.Data.Boundary as Boundary
import Webb.Boundary.Data.Method (Method)
import Webb.Boundary.Data.MethodTable (MethodTable)
import Webb.Boundary.Data.MethodTable as MethodTable
import Webb.Boundary.Data.Utils (findDuplicatesWith)

{- Build the boundary table. Helps verify that boundaries and methods aren't 
  declared more than once, by checking for duplicates easily, and making it 
  easy to lookup each method or boundary by name. Boundaries and Methods themselves are
  defined well-enough to transpile them with ease.
-}

newtype BoundaryTable = BT_ Table_

type BTable = BoundaryTable

derive instance Newtype BoundaryTable _
derive newtype instance Eq BoundaryTable
derive newtype instance Ord BoundaryTable
derive newtype instance Show BoundaryTable

type Table_ = 
  { array :: Array BoundDef
  , map :: Map String BoundDef
  }

_array :: forall a r. Lens' { array :: a | r } a
_array = prop (Proxy :: Proxy "array")

_map :: forall a r. Lens' { map :: a | r } a
_map = prop (Proxy :: Proxy "map")

type BoundDef = 
  { boundary :: Boundary
  , methods :: MethodTable
  }

_boundary :: forall a r. Lens' { boundary :: a | r } a
_boundary = prop (Proxy :: Proxy "boundary")

_methods :: forall a r. Lens' { methods :: a | r } a
_methods = prop (Proxy :: Proxy "methods")

emptyTable :: BoundaryTable
emptyTable = BT_ { map: Map.empty, array: [] }

addBoundary :: Boundary -> BoundaryTable -> BoundaryTable
addBoundary b table = let 
  s = unwrap table :: Table_
  methods = MethodTable.fromBoundary b :: MethodTable
  def = { boundary: b, methods } :: BoundDef
  in wrap $ s
  { array = Array.snoc s.array def
  , map = Map.insert (Bound.name b) def s.map
  }
  
insert :: Boundary -> BoundaryTable -> BoundaryTable
insert = addBoundary
  
lookup :: String -> BoundaryTable -> Maybe Boundary
lookup s = unwrap >>> view _map >>> Map.lookup s >>> map _.boundary

member :: String -> BoundaryTable -> Boolean
member s = unwrap >>> view _map >>> Map.member s

boundaries :: BoundaryTable -> Array Boundary
boundaries = unwrap >>> view _array >>> map _.boundary

duplicates :: BoundaryTable -> Array Boundary
duplicates table = let 
  array = boundaries table
  in findDuplicatesWith Boundary.name array
  
duplicateMethods :: BoundaryTable -> Array Method
duplicateMethods table = let 
  methods' = methodTables table
  dups = MethodTable.duplicates <$> methods'
  in join dups
  
methodTables :: BoundaryTable -> Array MethodTable
methodTables table = let 
  defs = table # unwrap >>> _.array
  in defs <#> _.methods

lookupMethod :: String -> String -> BoundaryTable -> Maybe Method
lookupMethod bname mname table = do
  let map = table # unwrap >>> _.map
  def <- Map.lookup bname map
  MethodTable.lookup mname def.methods

memberMethod :: String -> String -> BoundaryTable -> Boolean
memberMethod bname mname table = isJust $ lookupMethod bname mname table