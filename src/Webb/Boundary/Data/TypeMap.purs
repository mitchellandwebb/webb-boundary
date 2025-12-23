module Webb.Boundary.Data.TypeMap where

import Prelude

import Data.Array as Array
import Data.Map (Map)
import Data.Map as Map
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Tuple (Tuple, uncurry)
import Data.Tuple.Nested ((/\))
import Webb.Boundary.Data.Param (Param)
import Webb.Boundary.Data.Token (Token)
import Webb.Boundary.Data.Token as Token

newtype TypeMap = TM TypeMap_

type TypeMap_ = Map Token Param

derive newtype instance Eq TypeMap
derive newtype instance Ord TypeMap
derive newtype instance Show TypeMap
derive instance Newtype TypeMap _

newTypeMap :: TypeMap_ -> TypeMap
newTypeMap = wrap

params :: TypeMap -> Array Param
params = unwrap >>> Map.values >>> Array.fromFoldable

keys :: TypeMap -> Array String
keys = unwrap >>> Map.keys >>> Array.fromFoldable >>> map Token.text

keyTokens :: TypeMap -> Array Token
keyTokens = unwrap >>> Map.keys >>> Array.fromFoldable

asMap :: TypeMap -> Map Token Param
asMap = unwrap

pairs :: TypeMap -> Array (Tuple Token Param)
pairs = unwrap >>> Map.toUnfoldable

recordPairs :: TypeMap -> Array (Tuple String Param)
recordPairs m = m # pairs >>> map (uncurry convert)
  where
  convert token param = Token.text token /\ param
