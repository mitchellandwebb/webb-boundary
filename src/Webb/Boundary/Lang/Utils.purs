module Webb.Boundary.Lang.Utils where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Webb.Boundary.Data.Param (Param)
import Webb.Boundary.Data.Param as Param
import Webb.Boundary.Data.SymbolTable (SymbolTable)
import Webb.Boundary.Data.SymbolTable as STable
import Webb.Boundary.Gen.Param (GParam)
import Webb.Boundary.Gen.Param as GParam
import Webb.Monad.Prelude (forceMaybe')
import Webb.Stateful (localEffect)


convertParam :: SymbolTable -> (String -> String) -> Param -> GParam
convertParam table lookup param = 
  if Param.isEffect param || Param.isAff param then let
    arg = localEffect do 
      let mparam = Array.index (Param.args param) 0
      forceMaybe' "Missing a type parameter" mparam
    in convertParam table lookup arg
  else let
    -- We have to convert the parameter (which is complete) to a complete Kotlin type.
    -- If there are args, then the top symbol is NOT an alias; it can't be.
    -- If there are no args, the top symbol could be an alias, or not.
    name = Param.name param 
    args = Param.args param
  in if args == [] then let
    mparam = convertLoneSymbol table name
  in localEffect do 
    forceMaybe' ("Could not convert a type: " <> name) mparam
  else let 
    name' = lookup name :: String
    args' = (\arg -> convertParam table lookup arg) <$> args :: Array GParam.GParam
  in wrap
    { name: name'
    , args: args'
    }

-- Convert the lone symbol into a parameter
convertLoneSymbol :: SymbolTable -> String -> Maybe GParam
convertLoneSymbol table sym = do
  if STable.isAlias sym table then do
    pure $ wrap { name: sym, args: [] }
  else do
    Nothing
