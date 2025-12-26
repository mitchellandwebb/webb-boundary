module Webb.Boundary.Compile.Typescript where

import Prelude

import Data.Newtype (class Newtype, wrap)


newtype TypescriptParams = KP TypescriptParams_

type TsParams = TypescriptParams

type TypescriptParams_ =
  { 
  }
  
derive newtype instance Eq TypescriptParams
derive newtype instance Ord TypescriptParams
derive newtype instance Show TypescriptParams
derive instance Newtype TypescriptParams _
  
newTypescriptParams :: TypescriptParams_ -> TypescriptParams
newTypescriptParams = wrap

newTsParams :: TypescriptParams_ -> TypescriptParams
newTsParams = newTypescriptParams

