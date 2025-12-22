module Webb.Boundary.Data.Alias where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Show.Generic (genericShow)
import Webb.Boundary.Data.Param (Param)
import Webb.Boundary.Data.Token (Token)
import Webb.Boundary.Data.Token as Token
import Webb.Boundary.Data.TypeMap (TypeMap)


newtype Alias = A Alias_

type Alias_ =
  { name :: Token
  , target :: AliasTarget
  }
  
derive newtype instance Eq Alias
derive newtype instance Ord Alias
derive newtype instance Show Alias
derive instance Newtype Alias _
  
newAlias :: Alias_ -> Alias
newAlias = wrap
  
data AliasTarget = AliasedParam Param | AliasedMap TypeMap

derive instance Eq AliasTarget
derive instance Ord AliasTarget
derive instance Generic AliasTarget _
instance Show AliasTarget where show = genericShow

name :: Alias -> String
name = unwrap >>> _.name >>> Token.text

nameToken :: Alias -> Token
nameToken = unwrap >>> _.name 

target :: Alias -> AliasTarget
target = unwrap >>> _.target