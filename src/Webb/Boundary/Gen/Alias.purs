module Webb.Boundary.Gen.Alias 
( GAlias
, name, param, fromAlias
)
where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap, wrap)
import Webb.Boundary.Data.Alias (Alias)
import Webb.Boundary.Data.Alias as Alias
import Webb.Boundary.Gen.Lang (GParam)
import Webb.Boundary.Gen.Lang as Lang


{- Conversion to kotlin type alias -}

type GAlias = Lang.GAlias

name :: GAlias -> String
name = unwrap >>> _.name

param :: GAlias -> GParam
param = unwrap >>> _.param

fromAlias :: Lang.Lang_ -> Alias -> Maybe GAlias
fromAlias cv alias = do 
  let 
    name' = Alias.name alias
    target = Alias.target alias
  case target of
    Alias.AliasedParam p -> do
      let param' = Lang.param cv p
      pure $ wrap { name: name', param: param' }
    Alias.AliasedMap _ -> do
      Nothing
