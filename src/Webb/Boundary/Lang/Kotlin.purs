module Webb.Boundary.Lang.Kotlin where

import Prelude

import Data.Array as Array
import Data.Foldable as Fold
import Data.Map (Map)
import Data.Map as Map
import Data.Newtype (class Newtype, unwrap)
import Data.String as String
import Data.Tuple (uncurry)
import Data.Tuple.Nested ((/\))
import Webb.Boundary.Data.Param (Param)
import Webb.Boundary.Data.SymbolTable (STable)
import Webb.Boundary.Gen.Alias as GAlias
import Webb.Boundary.Gen.Interface as GInt
import Webb.Boundary.Gen.Lang (Lang_)
import Webb.Boundary.Gen.Lang as Lang
import Webb.Boundary.Gen.Method as GMethod
import Webb.Boundary.Gen.Param as GParam
import Webb.Boundary.Gen.Record as GRec
import Webb.Boundary.Lang.Utils (convertParam)
import Webb.Monad.Prelude (forceMaybe')
import Webb.Stateful (localEffect)
import Webb.Writer as Writer

newtype Kotlin = K STable

derive instance Newtype Kotlin _

kotlin :: STable -> Kotlin
kotlin table = K table

kotlinLang :: STable -> Lang_
kotlinLang table = Lang.wrap $ kotlin table

symbolTable :: Kotlin -> STable
symbolTable = unwrap

instance Lang.Lang Kotlin where
  param = param_
  writeParam _ = writeParam_
  writeMethod _ = writeMethod_
  writeInterface _ = writeInterface_
  writeRecord _ = writeRecord_
  writeAlias _ = writeAlias_
  
writeAlias_ :: GAlias.GAlias -> String
writeAlias_ alias = 
  "typealias " <> GAlias.name alias <> " = " <> writeParam_ (GAlias.param alias)
  
writeRecord_ :: GRec.GRecord -> String
writeRecord_ kdc = let
  pStrings = String.joinWith ", " $ GRec.params kdc <#> uncurry convert
  in "data class " <> GRec.name kdc <> "(" <> pStrings <> ")"
  where
  convert n param = "val " <> n <> ": " <> writeParam_ param
  
-- Convert the interface into a multiline string.
writeInterface_ :: GInt.GInterface -> String
writeInterface_ kt = let
  methodStrings = writeMethod_ <$> GInt.methods kt
  in Writer.runToString do 
    Writer.words ["interface", GInt.name kt, "{"]
    Writer.newline
    Writer.indent 2 do
      Writer.newline
      Fold.for_ methodStrings \m -> do
        Writer.write m
        Writer.newline
    Writer.word "}"
  
-- Print the parameter as a string, like `List<A, B>` or `Int`
writeParam_ :: GParam.GParam -> String
writeParam_ param = let
  args = GParam.args param
  argStrings = writeParam_ <$> GParam.args param
  argString = String.joinWith ", " argStrings
  in Writer.runToString do  
    Writer.write (GParam.name param)
    when (Array.length args > 0) do
      Writer.write "<"
      Writer.write argString
      Writer.write ">"
  
-- Convert the method to a bare string. No indentation is assumed.
writeMethod_ :: GMethod.GMethod -> String
writeMethod_ method = let
  argStrings = String.joinWith ", " (GMethod.args method <#> uncurry convertArg)
  returnString = writeParam_ $ GMethod.return method
  in Writer.runToString do
    when (GMethod.isAsync method) do
      Writer.word "suspend" 
    Writer.words ["fun", GMethod.name method]
    Writer.write "("
    Writer.write argStrings 
    Writer.write "):"
    Writer.word returnString
  where
  convertArg name' kparam = 
    name' <> ": " <> writeParam_ kparam

-- Represent our parameter as a fully-recursive kotlin parameter.
-- Ignore "Effect" and "Aff" when present, since they don't translate to Kotlin.
param_ :: Kotlin -> Param -> GParam.GParam
param_ cv param = convertParam table lookup param
  where
  table = symbolTable cv
  lookup = kotlinLookup cv

kotlinLookup :: Kotlin -> String -> String
kotlinLookup _cv str = localEffect do
  let mname = Map.lookup str mapper
  forceMaybe' ("Could not find kotlin name for: " <> str) mname

mapper :: Map String String
mapper = Map.fromFoldable
  [ "Int" /\ "Int"
  , "Number" /\ "Double"
  , "Double" /\ "Double"
  , "String" /\ "String"
  , "Array" /\ "List"
  , "Map" /\ "Map"
  , "Tuple" /\ "Pair"
  , "Unit" /\ "Unit"
  ]