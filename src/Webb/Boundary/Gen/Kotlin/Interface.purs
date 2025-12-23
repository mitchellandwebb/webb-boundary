module Webb.Boundary.Gen.Kotlin.Interface where

import Prelude

import Data.Foldable as Fold
import Data.Newtype (class Newtype, unwrap)
import Webb.Boundary.Data.Boundary as Bd
import Webb.Boundary.Gen.Kotlin.Param (KParam)
import Webb.Writer as Writer

{- Conversion of a boundary to a kotlin interface. -}

newtype KotlinInterface = KI
  { name :: String
  , methods :: Array KotlinMethod
  }
  
type KInterface = KotlinInterface
  
type KotlinMethod = 
  { name :: String
  , args :: Array KParam
  , return :: KParam
  , isAsync :: Boolean
  }
  
derive newtype instance Eq KotlinInterface
derive newtype instance Ord KotlinInterface
derive newtype instance Show KotlinInterface
derive instance Newtype KotlinInterface _

name :: KInterface -> String
name = unwrap >>> _.name

methods :: KInterface -> Array KotlinMethod
methods = unwrap >>> _.methods

fromBoundary :: Bd.Boundary -> KInterface
fromBoundary = unit

-- Convert the interface into a multiline string.
asKotlinString :: KInterface -> String
asKotlinString kt = let
  methodStrings = KMethod.asKotlinString <$> methods kt
  in do 
    Writer.words ["interface", name kt, "{"]
    Writer.newline
    Writer.indent 2 do
      Writer.newline
      Fold.for_ methodStrings \m ->
        Writer.write m
        Writer.newline
    Writer.word "}"
  