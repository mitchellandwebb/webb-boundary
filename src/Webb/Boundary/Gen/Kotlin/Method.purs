module Webb.Boundary.Gen.Kotlin.Method where

import Prelude

import Webb.Boundary.Gen.Kotlin.Param (KotlinParam)


{- Defines a kotlin method, how we obtain it, and how it becomes a string. -}


newtype KotlinMethod = KM 
  { name :: String
  , args :: Array KotlinParam
  , return :: KotlinParam
  , isAsync :: Boolean
  }
  
type KMethod = KotlinMethod