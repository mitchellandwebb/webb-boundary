module Webb.Boundary.Gen.Kotlin.DataClass where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.String as String
import Data.Tuple (uncurry)
import Data.Tuple.Nested ((/\), type (/\))
import Webb.Boundary.Data.Alias (Alias)
import Webb.Boundary.Data.Alias as Alias
import Webb.Boundary.Data.TypeMap as TypeMap
import Webb.Boundary.Gen.Kotlin.Convert (Converter)
import Webb.Boundary.Gen.Kotlin.Convert as Convert
import Webb.Boundary.Gen.Kotlin.Param (KParam)
import Webb.Boundary.Gen.Kotlin.Param as KParam


{- Conversions to the kotlin data class. -}

newtype KotlinDataClass = KDC 
  { name :: String
  , params :: Array (String /\ KParam)
  }

type KDataClass = KotlinDataClass
  
derive newtype instance Eq KotlinDataClass
derive newtype instance Ord KotlinDataClass
derive newtype instance Show KotlinDataClass
derive instance Newtype KotlinDataClass _

name :: KotlinDataClass -> String
name = unwrap >>> _.name

params :: KotlinDataClass -> Array (String /\ KParam)
params = unwrap >>> _.params

-- Convert any alias directly to a data class, _if_ it is a proper record.
fromAlias :: Alias -> Converter -> Maybe KDataClass
fromAlias alias cv = do
  let 
    name' = Alias.name alias
    target = Alias.target alias
  case target of
    Alias.AliasedMap m -> do
      let 
        pairs = TypeMap.recordPairs m
        params' = pairs <#> uncurry convert
      pure $ wrap { name: name', params: params' }
    _ -> do
      Nothing
  where
  convert n param = n /\ Convert.kotlinParam param cv
  
asKotlinString :: KDataClass -> String
asKotlinString kdc = let
  pStrings = String.joinWith ", " $ params kdc <#> uncurry convert
  in "data class " <> name kdc <> "(" <> pStrings <> ")"
  where
  convert n param = "val " <> n <> ": " <> KParam.asKotlinString param