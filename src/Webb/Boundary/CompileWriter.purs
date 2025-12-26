module Webb.Boundary.CompileWriter where

import Prelude


{-
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
-}