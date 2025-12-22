module Webb.Boundary.Tokens where

import Webb.Boundary.Prelude

import Data.Either (Either(..))
import Effect.Class (class MonadEffect, liftEffect)
import Parsing.String (eof)
import Parsing.String.Basic (skipSpaces)
import Webb.Boundary.Data.Token (Token, TokenKind(..))
import Webb.Boundary.Data.Token as Token
import Webb.Monad.Prelude (throwString)


{- We tokenize, but our goal is only to remove whitespace. That's it. 

The problem is that having a token type lets us _identify_ what the token
represents, so we don't have to parse later. Honestly, it's really bizarre how
bad this tool is for saying what I actually want to say.
-}

type Parse = Parser String

tokens :: forall m. MonadEffect m => String -> m (Array Token)
tokens input = do 
  let e = runParser input do 
        skipSpaces
        ts <- sepBy token skipSpaces
        skipSpaces
        eof
        pure ts

  case e of 
    Left (ParseError msg (Position s)) -> do
      throwString (show s <> ": " <> msg) # liftEffect
    Right ts -> do
      pure ts
      
token :: Parse Token
token = longest
  [ boundary
  , where'
  , operator
  , delim
  , separator
  , alias
  , typeName
  , functionName
  ]

asToken :: TokenKind -> Parse String -> Parse Token
asToken kind prog = try do 
  p1 <- filePosition
  i <- index
  string <- try prog
  p2 <- filePosition
  pure $ Token.new
    { kind
    , string 
    , line: p1.line
    , column: p1.column
    , endLine: p2.line
    , endColumn: p2.column
    , index: i
    }
  
boundary :: Parse Token
boundary = asToken Boundary do s "boundary"

where' :: Parse Token
where' = asToken Where do s "where"

operator :: Parse Token
operator = asToken Operator do 
  strings <- mix [ s "=", s ":", s "-", s ">", s "<" ]
  joined strings

delim :: Parse Token
delim = asToken Delim do 
  string <- alts [ s "{-", s "-}", s "{", s "}", s "(", s ")" ]
  pure string

separator :: Parse Token
separator = asToken Delim do 
  string <- alts [ s "," ]
  pure string

alias :: Parse Token 
alias = asToken Alias do s "type"

typeName :: Parse Token
typeName = asToken TypeName do
  first :: String <- upperChar
  rest :: Array String <- mix [ alphaChar, numChar, s "_" ]
  joined $ [first] <> rest

functionName :: Parse Token
functionName = asToken FunctionName do
  first <- lowerChar
  rest <- mix [alphaChar, numChar, s "_"]
  joined $ [first] <> rest