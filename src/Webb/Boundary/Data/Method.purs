module Webb.Boundary.Data.Method where

import Prelude

import Data.Array as Array
import Data.Maybe (fromMaybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Webb.Boundary.Data.Param (Param)
import Webb.Boundary.Data.Token (Token)
import Webb.Boundary.Data.Token as Token
import Webb.Monad.Prelude (forceMaybe')
import Webb.Stateful (localEffect)


newtype Method = M Method_

type Method_ =
  { name :: Token
  , params :: Array Param
  }
  
newMethod :: Method_ -> Method
newMethod = wrap
  
derive newtype instance Eq Method
derive newtype instance Ord Method
derive newtype instance Show Method
derive instance Newtype Method _

name :: Method -> String
name = unwrap >>> _.name >>> Token.text

nameToken :: Method -> Token
nameToken = unwrap >>> _.name

params :: Method -> Array Param
params = unwrap >>> _.params

firstParams :: Method -> Array Param
firstParams method = localEffect do
  let minit = Array.init $ params method
  pure $ fromMaybe [] minit
  
returnParam :: Method -> Param
returnParam method = localEffect do
  let mlast = Array.last $ params method
  forceMaybe' "No function parameters were provided" mlast