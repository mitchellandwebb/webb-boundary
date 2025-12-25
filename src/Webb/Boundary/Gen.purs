module Webb.Boundary.Gen where

import Webb.Boundary.Prelude

import Control.Alt ((<|>))
import Webb.Boundary.Data.Alias (Alias)
import Webb.Boundary.Data.Boundary (Boundary)
import Webb.Boundary.Gen.Alias as GAlias
import Webb.Boundary.Gen.Interface as GInt
import Webb.Boundary.Gen.Lang as Lang
import Webb.Boundary.Gen.Record as GRecord
import Webb.Monad.Prelude (forceMaybe')
import Webb.Stateful (localEffect)


{- Generating final strings using a language
-}

fromAlias :: Lang.Lang_ -> Alias -> String
fromAlias lang alias = let
  malias = Lang.writeAlias lang <$> GAlias.fromAlias lang alias :: _ String
  mrecord = Lang.writeRecord lang <$> GRecord.fromAlias lang alias :: _ String
  mcode = malias <|> mrecord
  in localEffect do
    forceMaybe' ("Unknown alias" <> show alias) mcode
    

fromBoundary :: Lang.Lang_ -> Boundary -> String
fromBoundary lang bound = let 
  interface = GInt.fromBoundary lang bound
  in Lang.writeInterface lang interface
