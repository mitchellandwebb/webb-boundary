module Webb.Boundary.Parser where

import Prelude

import Control.Monad.Error.Class (catchError)
import Data.Either (Either(..))
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Exception (message)
import Webb.Boundary.Data.Token (Token)
import Webb.Boundary.Data.Tree (Tree)
import Webb.Boundary.Parser.Parser as Parser
import Webb.Boundary.Parser.Tokens as Tokens


{- The parser object. It provides methods for understanding a _string_ that
represents the program -- understanding it as a stream of tokens or as a parse
tree. It is _stateless_ at the moment, but provides easy access to tokens and
the tree. 
-}

newtype Parser = P Unit

newParser :: forall m. MonadAff m => m Parser
newParser = do
  pure $ P unit
  
-- Tokenize the string
tokenize :: forall m. MonadAff m => 
  Parser -> String -> m (Either (Array String) (Array Token))
tokenize _ string = liftAff do
  catchError (do
    tokens <- Tokens.tokens string
    pure $ Right tokens
  ) (\e -> do 
    pure $ Left [ message e ]
  )

-- Turn the tokens into a Tree data structure.
parse :: forall m. MonadAff m => 
  Parser -> Array Token -> m (Either (Array String) Tree)  
parse _ tokens = liftAff do 
  e <- Parser.parseTokens tokens Parser.treeParser
  case e of 
    Left msg -> pure $ Left [ msg ]
    Right result -> pure $ Right result
