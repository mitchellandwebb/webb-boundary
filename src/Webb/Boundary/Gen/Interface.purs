module Webb.Boundary.Gen.Interface 
( GInterface
, name, methods, fromBoundary
)
where

import Prelude

import Data.Newtype (unwrap, wrap)
import Webb.Boundary.Data.Boundary as Bd
import Webb.Boundary.Data.Method (Method)
import Webb.Boundary.Gen.Lang (GMethod)
import Webb.Boundary.Gen.Lang as Lang
import Webb.Boundary.Gen.Method as GMethod

{- Conversion of a boundary to a kotlin interface. -}

type GInterface = Lang.GInterface

name :: GInterface -> String
name = unwrap >>> _.name

methods :: GInterface -> Array GMethod
methods = unwrap >>> _.methods

-- Create the interface from a boundary object's data.
fromBoundary :: Lang.Lang_ -> Bd.Boundary -> GInterface
fromBoundary cv b = let
  methods' = Bd.methods b :: Array Method
  kmethods = GMethod.fromMethod cv <$> methods' 
  in wrap { name: Bd.name b, methods: kmethods }
  
