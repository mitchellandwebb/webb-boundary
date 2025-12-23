module Webb.Boundary.Gen.Kotlin.Method where

import Prelude

import Data.FunctorWithIndex (mapWithIndex)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.String as String
import Data.Tuple (uncurry)
import Data.Tuple.Nested ((/\), type (/\))
import Webb.Boundary.Data.Method (Method)
import Webb.Boundary.Data.Method as Method
import Webb.Boundary.Data.Param (Param)
import Webb.Boundary.Gen.Kotlin.Convert (Converter)
import Webb.Boundary.Gen.Kotlin.Convert as Convert
import Webb.Boundary.Gen.Kotlin.Param (KotlinParam)
import Webb.Boundary.Gen.Kotlin.Param as KParam
import Webb.State.Prelude (newShowRef)
import Webb.Stateful (localEffect)


{- Defines a kotlin method, how we obtain it, and how it becomes a string. -}


newtype KotlinMethod = KM 
  { name :: String
  , args :: Array MethodParam
  , return :: KotlinParam
  , isAsync :: Boolean
  }
  
type KMethod = KotlinMethod

type MethodParam = String /\ KotlinParam

derive newtype instance Eq KotlinMethod
derive newtype instance Ord KotlinMethod
derive newtype instance Show KotlinMethod
derive instance Newtype KotlinMethod _

name :: KMethod -> String
name = unwrap >>> _.name

args :: KMethod -> Array MethodParam
args = unwrap >>> _.args

return :: KMethod -> KotlinParam
return = unwrap >>> _.return

isAsync :: KMethod -> Boolean
isAsync = unwrap >>> _.isAsync

fromMethod :: Method -> Converter -> KMethod
fromMethod method cv = let
  name' = Method.name method
  args' = Method.firstParams method
  return' = Method.returnParam method 
  kargs' = mapWithIndex convertArg args'
  kreturn' = Convert.kotlinParam return' cv
  isAsync' = Convert.isAsync return' cv
  in wrap
    { name: name'
    , args: kargs'
    , return: kreturn'
    , isAsync: isAsync'
    }
  where
  convertArg :: Int -> Param -> MethodParam
  convertArg i param = let
    name' = "a" <> show i
    kparam = Convert.kotlinParam param cv
    in name' /\ kparam

-- Convert the method to a bare string. No indentation is assumed.
asKotlinString :: KMethod -> String
asKotlinString method = let
  argStrings = String.joinWith ", " (args method <#> uncurry convertArg)
  returnString = KParam.asKotlinString $ return method
  in do
    when (isAsync method) do
      Writer.word "suspend" 
    Writer.words ["fun", name method]
    Writer.write "("
    Writer.write argStrings 
    Writer.write "):"
    Writer.word returnString
  where
  convertArg name kparam = 
    name <> ": " <> KParam.asKotlinString kparam