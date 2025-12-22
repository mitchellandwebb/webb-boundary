module Webb.Boundary.Parser where

import Webb.Boundary.Prelude

import Data.List (List)
import Data.Map (Map)
import Data.Map as Map
import Parsing.Token as T
import Webb.Boundary.Data.Param (Param)
import Webb.Boundary.Data.Param as Param
import Webb.Boundary.Data.Token (Token, TokenKind)
import Webb.Boundary.Data.Token as Tok

type Parse = Parser (List Token)

-- Get the next token.
next :: Parse Token
next = try do T.token getPos
  where
  getPos t = let 
    p = Tok.position t
    in Position 
      { index: p.index
      , column: p.column
      , line: p.line
      }
    
-- Get the next token, while asserting it is a specific kind of token.
token :: TokenKind -> Parse Token
token kind = try do 
  t <- next
  if Tok.kind t == kind then do
    pure t
  else do
    fail $ "Expected token " <> show kind <> ", but got " <> show (Tok.kind t)

op :: String -> Parse Token
op str = try do 
  t <- token Tok.Operator
  if Tok.hasText str t then do
    pure t
  else do
    fail $ "Expected token " <> str <> ", but got " <> Tok.text t

delim :: String -> Parse Token
delim str = try do 
  t <- token Tok.Delim
  if Tok.hasText str t then do
    pure t
  else do
    fail $ "Expected token " <> str <> ", but got " <> Tok.text t

sep :: String -> Parse Token
sep str = do 
  t <- token Tok.Separator
  if Tok.hasText str t then do
    pure t
  else do
    fail $ "Expected token " <> str <> ", but got " <> Tok.text t
    
type Boundary = 
  { name :: Token
  , methods :: Array Method
  }
    
-- Parse a boundary definition.
boundary :: Parse Boundary
boundary = try do 
  _ <- token Tok.Boundary 
  name <- token Tok.TypeName
  _ <- token Tok.Where
  methods <- many method
  
  pure 
    { name: name
    , methods
    }
    
type Method = 
  { name :: Token
  , params :: Array Param
  }

method :: Parse Method
method = try do
  name <- token Tok.FunctionName
  _ <- op "::"
  params <- sepBy param (op "->")
  pure { name, params }
  
-- Int | (Int) | Array Int | Array (Int) | (Array) Int | (Array (Array Int)) | Array Array Int
param :: Parse Param
param = try do 
  strip lp rp do
    name <- token Tok.TypeName
    args <- many (paramArg unit)
    pure $ Param.newParam { name, args }
    
  where
  lp = delim  "("
  rp = delim ")"
  
  -- A param argument. It is either a bare param, or delimited to become a top-level param
  -- again.
  paramArg _ = do
    alts 
      [ do
          name <- token Tok.TypeName
          pure $ Param.newParam { name, args: [] }
      , whenNext lp param 
      ]

  
type Alias = 
  { name :: Token
  , target :: AliasTarget
  }
  
data AliasTarget = AliasedParam Param | AliasedMap TypeMap

-- Parse a type alias
alias :: Parse Alias
alias = try do
  _ <- token Tok.Alias
  name <- token Tok.TypeName
  _ <- op "="
  target <- alts [ AliasedParam <$> param, AliasedMap <$> typeMap ]
  pure { name, target }
  
type TypeMap = Map Token Param

-- Parse a type map -- in other words, a Record type
typeMap :: Parse TypeMap
typeMap = try do 
  _ <- delim "{"
  pairs <- sepEndBy pair (sep ",")
  _ <- delim "}"
  pure $ Map.fromFoldable pairs

  where
  pair = try do
    name <- token Tok.FunctionName
    _ <- op "::"
    p <- param
    pure $ name /\ p
    
