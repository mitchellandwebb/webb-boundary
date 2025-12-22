module Webb.Boundary.Gen where

import Webb.Boundary.Prelude
import Webb.Boundary.Tree

import Control.Monad.Except (ExceptT, runExceptT)
import Data.Array as Array
import Data.Either (Either(..))
import Effect.Aff (Aff, throwError)
import Effect.Aff.Class (liftAff)
import Webb.Boundary.BoundarySymbols as BSymbols
import Webb.Boundary.Data.Alias (Alias)
import Webb.Boundary.Data.Alias as Alias
import Webb.Boundary.Data.Boundary (Boundary)
import Webb.Boundary.Data.BoundaryTable (BTable)
import Webb.Boundary.Data.BoundaryTable as BTable
import Webb.Boundary.Data.SymbolTable (STable)
import Webb.Boundary.Data.SymbolTable as STable
import Webb.Boundary.Parser as Parser
import Webb.Boundary.Tokens as Tokens
import Webb.Boundary.Tree as Tree
import Webb.Boundary.TypeCheck as TypeCheck
import Webb.Boundary.TypeSymbols as TypeSymbols
import Webb.Monad.Prelude (forceMaybe')
import Webb.Stateful (localEffect)


{- Define the data structures and functions that will be used by generators to build code.
-}

type Env = 
  { symbols :: STable
  , boundaries :: BTable
  , tree :: Tree
  }
  
-- Either we are ready to generate, or we obtain errors.
buildEnv :: String -> Aff (Either (Array String) Env)
buildEnv file = runExceptT do
  tokens <- Tokens.tokens file
  tree <- abort' $ Parser.parseTokens tokens Tree.treeParser
  symbols <- abort $ TypeSymbols.buildSymbolTable tree
  boundaries <- abort $ BSymbols.buildBoundaryTable tree symbols
  _ <- abort $ TypeCheck.runTypeCheck symbols tree
  let env = { tree, symbols, boundaries }
  pure env

type Prog = ExceptT (Array String) Aff 

abort' :: forall a. Aff (Either String a) -> Prog a
abort' prog = do
  either <- liftAff prog
  case either of
    Left s -> do throwError [s]
    Right a -> pure a

abort :: forall a. Aff (Either (Array String) a) -> Prog a
abort prog = do
  either <- liftAff prog
  case either of
    Left errors -> do throwError errors
    Right a -> pure a

-- From the environment, obtain an array of the declared aliases
getAliases :: Env -> Aff (Array Alias)
getAliases env = do
  let aliases = STable.aliases env.symbols
  pure aliases

-- Get the boundaries. We can iterate over the data type using the contained knowledge, 
-- to get all the data we need to write the data.
getBoundaries :: Env -> Aff (Array Boundary)
getBoundaries env = do
  let bounds = BTable.boundaries env.boundaries
  pure bounds
  
-- Sort the aliases in order of their dependency on each other. Aliases with
-- fewer dependencies will come first. This is useful if the generated code
-- requires earlier types to be earlier in the file.
sortAliases :: STable -> Array Alias -> Array Alias
sortAliases table arr = let
  tierList = STable.newTierList table
  in Array.sortWith (tier tierList) arr 
  where
  tier :: STable.AliasTierList -> Alias -> Int
  tier tierList alias = let
    mtier = STable.tier (Alias.name alias) tierList
    in localEffect do 
      forceMaybe' ("Alias wasn't found in any tier: " <> show (Alias.name alias)) mtier
