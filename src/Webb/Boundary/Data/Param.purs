module Webb.Boundary.Data.Param where

import Prelude

import Data.Array as Array
import Data.Foldable as Fold
import Data.Newtype (class Newtype, unwrap, wrap)
import Webb.Boundary.Data.Token (Token)
import Webb.Boundary.Data.Token as Token
import Webb.State.Prelude (aread)
import Webb.Stateful (localEffect)
import Webb.Stateful.ArrayColl as ArrayColl


{- Represents a full concrete type parameter, possibly with arguments -}


newtype Param = Param_ Param_

type Param_ = 
  { name :: Token
  , args :: Array Param
  }
  
derive instance Eq Param
derive instance Ord Param
derive instance Newtype Param _
instance Show Param where
  show (Param_ s) = show
    { name: s.name
    , args: s.args
    }
    
newParam :: Param_ -> Param
newParam = wrap

name :: Param -> String
name = unwrap >>> _.name >>> Token.text

args :: Param -> Array Param
args = unwrap >>> _.args

argCount :: Param -> Int
argCount = args >>> Array.length

symbols :: Param -> Array String
symbols param = localEffect do
  array <- ArrayColl.newArray
  ArrayColl.addLast array (name param)
  Fold.for_ (args param) \arg -> do
    ArrayColl.addAllLast array (symbols arg)
  aread array
  
isEffect :: Param -> Boolean
isEffect param = (name param == "Effect")

isAff :: Param -> Boolean
isAff param = (name param == "Aff")