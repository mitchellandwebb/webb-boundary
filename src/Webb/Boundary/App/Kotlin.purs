module Webb.Boundary.App.Kotlin where

import Prelude

{-
What ... model is needed to start writing the Kotlin to file? What shared
model? What language is needed? How do we merge with the need for Files and 
Dirs, or is there anything going on? What data is actually needed? Without
well-defined data, we can't know the scope of what we're doing. But we can only
proceed from what is _known_. 
  We can know more, or less. We might know all Options for a Command-Line target. But that's a lot of options, and probably too much. But what's more likely is that we need models for handling less information about what's happening. For example, we might only know about a single source file, or we might know about multiple source files and target files.
  But what's even more odd is that there isn't really ... just one way to do this. Different starting points of knowledge lead to different things we need to do to obtain the proper user experience. And even for CodeWriter, it's still not necessarily certain that we need to specify a _concrete file_ to existence. We could just write to an abstract file instead; in fact, we probably should to be compatible with more possibilities.
  Yet all these things are knowledge. Since this isn't algorithm implementation, there isn't any particular one that is preferable. But it _does_ mean that it's true that for any given language implementation, we do need to have the knowledge for what to do. But the question is then -- _what_ knowledge exists? If we don't know the kinds of knowledge we might have at any particular time, then how can we do anything? Are models predicated on knowledge? Or is knowledge predicated on models? Or are both particular kinds of knowledge, which we may or may not have, at any one time? Are we always writing to a file, or might we be writing to something else? For now, at least,
we can say that we're only writing to files. There's no clear point of knowledge. Thus, it only makes sense to go with the minimal amount of knowledge, and write code from there.
-}

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