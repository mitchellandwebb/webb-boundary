module Webb.Boundary.Gen.Kotlin.Alias where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap, wrap)
import Webb.Boundary.Data.Alias (Alias)
import Webb.Boundary.Data.Alias as Alias
import Webb.Boundary.Gen.Kotlin.Convert (Converter)
import Webb.Boundary.Gen.Kotlin.Convert as Convert
import Webb.Boundary.Gen.Kotlin.Param (KotlinParam)
import Webb.Boundary.Gen.Kotlin.Param as KParam


{- Conversion to kotlin type alias -}

newtype KotlinAlias = KA
  { name :: String
  , param :: KotlinParam
  }
  
type KAlias = KotlinAlias
  
derive newtype instance Eq KotlinAlias
derive newtype instance Ord KotlinAlias
derive newtype instance Show KotlinAlias
derive instance Newtype KotlinAlias _

name :: KotlinAlias -> String
name = unwrap >>> _.name

param :: KotlinAlias -> KotlinParam
param = unwrap >>> _.param

fromAlias :: Alias -> Converter -> Maybe KotlinAlias
fromAlias alias cv = do 
  let 
    name' = Alias.name alias
    target = Alias.target alias
  case target of
    Alias.AliasedParam p -> do
      let param' = Convert.kotlinParam p cv
      pure $ wrap { name: name', param: param' }
    Alias.AliasedMap _ -> do
      Nothing

asKotlinString :: KotlinAlias -> String
asKotlinString alias = 
  "typealias " <> name alias <> " = " <> KParam.asKotlinString (param alias)