module Webb.Boundary.Gen.Kotlin.Convert where

import Prelude

import Data.Array as Array
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Tuple.Nested ((/\))
import Webb.Boundary.Data.Param (Param)
import Webb.Boundary.Data.Param as Param
import Webb.Boundary.Data.SymbolTable (STable)
import Webb.Boundary.Data.SymbolTable as STable
import Webb.Boundary.Gen (Env)
import Webb.Boundary.Gen.Kotlin.Param (KotlinParam)
import Webb.Monad.Prelude (forceMaybe')
import Webb.Stateful (localEffect)

newtype Converter = CV Env

derive newtype instance Eq Converter 
derive newtype instance Ord Converter
derive instance Newtype Converter _
derive newtype instance Show Converter 

symbolTable :: Converter -> STable
symbolTable = unwrap >>> _.symbols

isAsync :: Param -> Converter -> Boolean
isAsync param _cv = Param.isAff param

kotlinParam :: Param -> Converter -> KotlinParam
kotlinParam = convert

-- Represent our parameter as a fully-recursive kotlin parameter.
-- Ignore "Effect" and "Aff" when present, since they don't translate to Kotlin.
convert :: Param -> Converter -> KotlinParam
convert param cv = 
  if Param.isEffect param || Param.isAff param then let
    arg = localEffect do 
      let mparam = Array.index (Param.args param) 0
      forceMaybe' "Missing a type parameter" mparam
    in convert arg cv
  else let
    -- We have to convert the parameter (which is complete) to a complete Kotlin type.
    -- If there are args, then the top symbol is NOT an alias; it can't be.
    -- If there are no args, the top symbol could be an alias, or not.
    name = Param.name param 
    args = Param.args param
  in if args == [] then let
    mparam = convertSymbol name cv
  in localEffect do 
    forceMaybe' ("Could not convert a type: " <> name) mparam
  else let 
    kname = kotlinName name cv :: String
    kargs = (\arg -> convert arg cv) <$> args :: Array KotlinParam
  in wrap
    { name: kname
    , args: kargs
    }
    
-- Take a symbol and attempt to convert it to a full kotlin parameter.
-- Fails if it is anything but an Alias to a complete type. Note that since aliases
-- ARE defined in Kotlin, it is sufficient to return the alias name.
convertSymbol :: String -> Converter -> Maybe KotlinParam
convertSymbol sym cv = do
  let table = symbolTable cv
  if STable.isAlias sym table then do
    pure $ wrap { name: sym, args: [] }
  else do
    Nothing
  
kotlinName :: String -> Converter -> String
kotlinName str _cv = localEffect do
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

