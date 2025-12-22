module Webb.Boundary.Data.Param where

import Prelude

import Data.Array as Array
import Data.Newtype (class Newtype, unwrap, wrap)
import Webb.Boundary.Data.Token (Token)
import Webb.Boundary.Data.Token as Token


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