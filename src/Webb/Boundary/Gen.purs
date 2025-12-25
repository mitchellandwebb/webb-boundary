module Webb.Boundary.Gen where

import Webb.Boundary.Data.Tree
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