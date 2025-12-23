module Webb.Boundary.Gen.Kotlin.Param where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype, unwrap)
import Data.String as String


newtype KotlinParam = KP KotlinParam_

type KParam = KotlinParam

type KotlinParam_ = 
  { name :: String
  , args :: Array KotlinParam
  }
  
derive instance Eq KotlinParam
derive instance Ord KotlinParam
derive instance Newtype KotlinParam _
derive instance Generic KotlinParam _
instance Show KotlinParam where
  show (KP param) = show
    { name: param.name
    , args: show param.args
    }

name :: KotlinParam -> String
name = unwrap >>> _.name

args :: KotlinParam -> Array KotlinParam
args = unwrap >>> _.args
  
-- Print the parameter as a string, like `List<A, B>` or `Int`
asKotlinString :: KotlinParam -> String
asKotlinString param = let 
  name' = name param
  args' = args param
  in if args' == [] then
    name'
  else let 
    -- Might be slow without a string builder, but oh well.
    argStrings = asKotlinString <$> args'   
    joined = String.joinWith ", " argStrings
  in name' <> "<" <> joined <> ">"