module Webb.Boundary.Gen.Param 
( name, args, GParam
)
where

import Prelude

import Data.Newtype (unwrap)
import Webb.Boundary.Gen.Lang as Lang

type GParam = Lang.GParam

name :: GParam -> String
name = unwrap >>> _.name

args :: GParam -> Array GParam
args = unwrap >>> _.args
  