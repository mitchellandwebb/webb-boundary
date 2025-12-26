module Webb.Boundary.Lang.Kotlin where

import Prelude

import Data.Array as Array
import Data.Foldable as Fold
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.String as String
import Data.Tuple (uncurry)
import Data.Tuple.Nested ((/\))
import Webb.Boundary.Data.Param (Param)
import Webb.Boundary.Data.Param as Param
import Webb.Boundary.Data.SymbolTable (STable)
import Webb.Boundary.Data.SymbolTable as STable
import Webb.Boundary.Gen.Alias as GAlias
import Webb.Boundary.Gen.Interface as GInt
import Webb.Boundary.Gen.Lang (Lang_)
import Webb.Boundary.Gen.Lang as Lang
import Webb.Boundary.Gen.Method as GMethod
import Webb.Boundary.Gen.Param as GParam
import Webb.Boundary.Gen.Record as GRec
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
writeParam_ param =
  if GParam.args param == [] then
    GParam.name param 
  else let 
    -- Might be slow without a string builder, but oh well.
    argStrings = writeParam_ <$> GParam.args param
    joined = String.joinWith ", " argStrings
  in GParam.name param <> "<" <> joined <> ">"
  
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
param_ cv param = 
  if Param.isEffect param || Param.isAff param then let
    arg = localEffect do 
      let mparam = Array.index (Param.args param) 0
      forceMaybe' "Missing a type parameter" mparam
    in param_ cv arg
  else let
    -- We have to convert the parameter (which is complete) to a complete Kotlin type.
    -- If there are args, then the top symbol is NOT an alias; it can't be.
    -- If there are no args, the top symbol could be an alias, or not.
    name = Param.name param 
    args = Param.args param
  in if args == [] then let
    mparam = convertSymbol cv name
  in localEffect do 
    forceMaybe' ("Could not convert a type: " <> name) mparam
  else let 
    kname = kotlinName cv name :: String
    kargs = (\arg -> param_ cv arg) <$> args :: Array GParam.GParam
  in wrap
    { name: kname
    , args: kargs
    }
    
-- Take a symbol and attempt to convert it to a full kotlin parameter.
-- Fails if it is anything but an Alias to a complete type. Note that since aliases
-- ARE defined in Kotlin, it is sufficient to return the alias name.
convertSymbol :: Kotlin -> String -> Maybe GParam.GParam
convertSymbol cv sym = do
  let table = symbolTable cv
  if STable.isAlias sym table then do
    pure $ wrap { name: sym, args: [] }
  else do
    Nothing
  
kotlinName :: Kotlin -> String -> String
kotlinName _cv str = localEffect do
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
  ]