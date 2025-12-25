module Webb.Boundary.Gen.Record 
( name, params, fromAlias, GRecord
)
where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap, wrap)
import Data.Tuple (uncurry)
import Data.Tuple.Nested ((/\))
import Webb.Boundary.Data.Alias (Alias)
import Webb.Boundary.Data.Alias as Alias
import Webb.Boundary.Data.TypeMap as TypeMap
import Webb.Boundary.Gen.Lang as Lang


{- Conversions to the kotlin data class. -}

type GRecord = Lang.GRecord

name :: GRecord -> String
name = unwrap >>> _.name

params :: GRecord -> Array Lang.RecordPair
params = unwrap >>> _.params

-- Convert any alias directly to a data class, _if_ it is a proper record.
fromAlias :: Lang.Lang_ -> Alias -> Maybe GRecord
fromAlias cv alias = do
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
  convert n param = n /\ Lang.param cv param
  