module Webb.Boundary.Lang.Typescript where

import Prelude

import Data.Array as Array
import Data.Foldable as Fold
import Data.FoldableWithIndex (forWithIndex_)
import Data.Map (Map)
import Data.Map as Map
import Data.String as String
import Data.Tuple.Nested ((/\))
import Unsafe.Coerce (unsafeCoerce)
import Webb.Boundary.Data.Param (Param)
import Webb.Boundary.Data.SymbolTable (STable)
import Webb.Boundary.Data.SymbolTable as STable
import Webb.Boundary.Gen.Alias (GAlias)
import Webb.Boundary.Gen.Alias as GAlias
import Webb.Boundary.Gen.Interface (GInterface)
import Webb.Boundary.Gen.Interface as GInt
import Webb.Boundary.Gen.Lang (Lang_)
import Webb.Boundary.Gen.Lang as Lang
import Webb.Boundary.Gen.Method (GMethod)
import Webb.Boundary.Gen.Method as GMethod
import Webb.Boundary.Gen.Param (GParam)
import Webb.Boundary.Gen.Param as GParam
import Webb.Boundary.Gen.Record (GRecord)
import Webb.Boundary.Gen.Record as GRecord
import Webb.Boundary.Lang.Utils (convertParam)
import Webb.Monad.Prelude (forceMaybe, forceMaybe')
import Webb.Stateful (localEffect)
import Webb.Writer (runToString)
import Webb.Writer as Writer


{- Define the standard components of the typescript language translator
-}

newtype Typescript = TS STable.STable

typescript :: STable -> Typescript
typescript table = TS table

tsLang :: STable.STable -> Lang_
tsLang table = Lang.wrap $ typescript table

instance Lang.Lang Typescript where
  param = param_ 
  writeParam = writeParam_
  writeMethod = writeMethod_
  writeInterface = writeInterface_
  writeRecord = writeRecord_
  writeAlias = writeAlias_
  
-- To turn the param into typescript types, we need a mapper and a symbol table to
-- help unwind aliases.
param_ :: Typescript -> Param -> GParam
param_ (TS table) = convertParam table lookupName
  where
  lookupName string = localEffect do
    forceMaybe' ("Unknown type: " <> string) $ Map.lookup string mapper

mapper :: Map String String
mapper = Map.fromFoldable
  [ "Int" /\ "number"
  , "Number" /\ "number"
  , "Double" /\ "number"
  , "String" /\ "string"
  , "Array" /\ "Array"
  , "Map" /\ "Record"
  , "Tuple" /\ "Tuple" -- `Tuple int number` must become `[int, number]`
  , "Unit" /\ "void"
  ]

-- Recursively write the parameters.
writeParam_ :: Typescript -> GParam -> String
writeParam_ self param = let 
  -- Remember that records are handled correctly -- they will be _aliased_,
  -- which means the string name is enough.
  name = GParam.name param
  args = GParam.args param
  argString = String.joinWith ", " (args <#> writeParam_ self)
  in if name == "Tuple" then let
    totalTemplate = """[${args}]"""
    in replace totalTemplate { args: argString }
  else let 
    totalTemplate = """${name}<${args}>"""
    in replace totalTemplate { args: argString, name }

writeMethod_ :: Typescript -> GMethod -> String
writeMethod_ self method = let 
  isAsync = GMethod.isAsync method
  totalTemplate = """
    |${async}${name}(${args}): ${return};
    """
  argTemplate = """${name}: ${kind}"""
  async = if isAsync then "async " else ""
  name = GMethod.name method
  args = String.joinWith ", " $ GMethod.args method \(name /\ kind) -> do
    replace argTemplate 
      { name
      , kind: writeParam_ self kind
      }
  return = writeParam_ self (GMethod.return method)
  in replace totalTemplate
    { async, name, args, return }
  
writeInterface_ :: Typescript -> GInterface -> String
writeInterface_ self int = let 
  totalTemplate = """
    |interface ${name} {
    |${methods}
    |}
    """
  methodTemplate = """
    |${method}
    """
  methods = String.joinWith "\n" $ GInt.methods int \method ->
    replace methodTemplate 
      { method: writeMethod_ self method
      }
      
  in replace totalTemplate 
    { name: GInt.name int
    , methods
    }

writeRecord_ :: Typescript -> GRecord -> String
writeRecord_ self rec = let
  totalTemplate = """
    |type ${name} = {
    |${pairs}
    |}
    """
  pairTemplate = """
      |  ${key}: ${value},
    """
  pairs = String.joinWith "\n" $ GRecord.params rec \param -> 
    replace pairTemplate 
      { key: GParam.name param
      , value: writeParam_ self $ GParam.args param
      }
  in replace totalTemplate 
    { name: GRecord.name rec
    , pairs
    }

writeAlias_ :: Typescript -> GAlias -> String
writeAlias_ self alias = let
  template = """
    |type ${name} = ${param}
  """
  param = writeParam_ self (GAlias.param alias)
  in replace template { name: GAlias.name alias, param: param }