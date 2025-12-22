module Webb.Boundary.Data.MethodTable where

import Prelude

import Data.Array as Array
import Data.Foldable as Fold
import Data.Lens (Lens', over)
import Data.Lens.Record (prop)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Type.Proxy (Proxy(..))
import Webb.Boundary.Data.Boundary (Boundary)
import Webb.Boundary.Data.Boundary as Bound
import Webb.Boundary.Data.Method (Method)
import Webb.Boundary.Data.Method as Method
import Webb.Boundary.Data.Utils (findDuplicatesWith)
import Webb.State.Prelude (views)


newtype MethodTable = MT Table_

type MTable = MethodTable

type Table_ = 
  { array :: Array Method
  , map :: Map String Method
  }

_array :: forall a r. Lens' { array :: a | r } a
_array = prop (Proxy :: Proxy "array")

_map :: forall a r. Lens' { map :: a | r } a
_map = prop (Proxy :: Proxy "map")
  
derive newtype instance Eq MethodTable
derive newtype instance Ord MethodTable
derive newtype instance Show MethodTable
derive instance Newtype MethodTable _

emptyTable :: MethodTable
emptyTable = wrap
  { array: []
  , map: Map.empty
  }
  
fromBoundary :: Boundary -> MethodTable
fromBoundary b = let
  methods' = Bound.methods b
  init = emptyTable
  in 
  Fold.foldl add_ init methods'
  where
  add_ table method = insert method table
  
methods :: MethodTable -> Array Method
methods = unwrap >>> _.array

addMethod :: Method -> MethodTable -> MethodTable
addMethod method = let 
  name = Method.name method
  in
  unwrap >>> 
    over _map (Map.insert name method) >>>
    over _array (flip Array.snoc method) >>> 
    wrap

insert :: Method -> MethodTable -> MethodTable
insert = addMethod
    
lookup :: String -> MethodTable -> Maybe Method
lookup name = unwrap >>> views _map (Map.lookup name)

-- Return the methods that were already defined before.
duplicates :: MethodTable -> Array Method
duplicates table = let
  array = methods table
  in findDuplicatesWith Method.name array