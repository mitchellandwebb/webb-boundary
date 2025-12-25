module Webb.Boundary.Gen where

import Webb.Boundary.Prelude
import Webb.Boundary.Data.Tree

import Control.Monad.Except (ExceptT, runExceptT)
import Data.Array as Array
import Data.Either (Either(..))
import Effect.Aff (Aff, throwError)
import Effect.Aff.Class (liftAff)
import Webb.Boundary.Analyzer.BoundarySymbols as BSymbols
import Webb.Boundary.Data.Alias (Alias)
import Webb.Boundary.Data.Alias as Alias
import Webb.Boundary.Data.Boundary (Boundary)
import Webb.Boundary.Data.BoundaryTable (BTable)
import Webb.Boundary.Data.BoundaryTable as BTable
import Webb.Boundary.Data.SymbolTable (STable)
import Webb.Boundary.Data.SymbolTable as STable
import Webb.Boundary.Parser.Parser as Parser
import Webb.Boundary.Parser.Tokens as Tokens
import Webb.Boundary.Data.Tree as Tree
import Webb.Boundary.Analyzer.TypeCheck as TypeCheck
import Webb.Boundary.Analyzer.TypeSymbols as TSymbols
import Webb.Monad.Prelude (forceMaybe')
import Webb.Stateful (localEffect)


{- Define the data structures and functions that will be used by generators 
  to build code.
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
  tree <- abort' $ Parser.parseTokens tokens Parser.treeParser
  symbols <- abort $ TSymbols.buildSymbolTable tree
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
