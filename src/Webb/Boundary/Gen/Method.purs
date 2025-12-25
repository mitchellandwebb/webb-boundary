module Webb.Boundary.Gen.Method 
( name, args, return, isAsync, fromMethod, GMethod
)
where

import Prelude

import Data.FunctorWithIndex (mapWithIndex)
import Data.Newtype (unwrap, wrap)
import Data.Tuple.Nested ((/\))
import Webb.Boundary.Data.Method (Method)
import Webb.Boundary.Data.Method as Method
import Webb.Boundary.Data.Param (Param)
import Webb.Boundary.Gen.Lang (GParam, Lang_)
import Webb.Boundary.Gen.Lang as Lang

{- Defines a kotlin method, how we obtain it, and how it becomes a string. -}
type GMethod = Lang.GMethod

name :: GMethod -> String
name = unwrap >>> _.name

args :: GMethod -> Array Lang.MethodParam
args = unwrap >>> _.args

return :: GMethod -> GParam
return = unwrap >>> _.return

isAsync :: GMethod -> Boolean
isAsync = unwrap >>> _.isAsync

fromMethod :: Lang_ -> Method -> GMethod
fromMethod cv method = let
  name' = Method.name method
  args' = Method.firstParams method
  return' = Method.returnParam method 
  kargs' = mapWithIndex convertArg args'
  kreturn' = Lang.param cv return'
  isAsync' = Lang.isAsync cv return'
  in wrap
    { name: name'
    , args: kargs'
    , return: kreturn'
    , isAsync: isAsync'
    }
  where
  convertArg :: Int -> Param -> Lang.MethodParam
  convertArg i param = let
    name' = "a" <> show i
    kparam = Lang.param cv param
    in name' /\ kparam
