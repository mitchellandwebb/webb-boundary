module Webb.Boundary.Data.Boundary where

import Prelude

import Data.Newtype (class Newtype, unwrap, wrap)
import Webb.Boundary.Data.Method (Method)
import Webb.Boundary.Data.Token (Token)
import Webb.Boundary.Data.Token as Token


newtype Boundary = B Boundary_

type Boundary_ = 
  { name :: Token
  , methods :: Array Method
  }
  
newBoundary :: Boundary_ -> Boundary
newBoundary = wrap

derive instance Newtype Boundary _
derive newtype instance Eq Boundary
derive newtype instance Ord Boundary
derive newtype instance Show Boundary

name :: Boundary -> String
name = unwrap >>> _.name >>> Token.text

nameToken :: Boundary -> Token
nameToken = unwrap >>> _.name

methods :: Boundary -> Array Method
methods = unwrap >>> _.methods

