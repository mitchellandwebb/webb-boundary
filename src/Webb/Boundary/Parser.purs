module Webb.Boundary.Parser where

import Webb.Boundary.Prelude

import Data.List (List)
import Data.Map (Map)
import Data.Map as Map
import Parsing.Token as T
import Webb.Boundary.Tokens (Kind(..), Token)


type Parse = Parser (List Token)

-- Get the next token.
next :: Parse Token
next = try do T.token getPos
  where
  getPos t = Position 
    { index: t.index
    , column: t.column
    , line: t.line
    }
    
-- Get the next token, while asserting it is a specific kind of token.
token :: Kind -> Parse Token
token kind = try do 
  t <- next
  if t.kind == kind then do
    pure t
  else do
    fail $ "Expected token " <> show kind <> ", but got " <> show t.kind

op :: String -> Parse Token
op str = try do 
  t <- token Operator
  if t.string == str then do
    pure t
  else do
    fail $ "Expected token " <> str <> ", but got " <> t.string

delim :: String -> Parse Token
delim str = try do 
  t <- token Delim
  if t.string == str then do
    pure t
  else do
    fail $ "Expected token " <> str <> ", but got " <> t.string

sep :: String -> Parse Token
sep str = do 
  t <- token Separator
  if t.string == str then do
    pure t
  else do
    fail $ "Expected token " <> str <> ", but got " <> t.string
    
type Boundary = 
  { name :: Token
  , methods :: Array Method
  }
    
-- Parse a boundary definition.
boundary :: Parse Boundary
boundary = try do 
  _ <- token Boundary 
  name <- token TypeName
  _ <- token Where
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
  name <- token FunctionName
  _ <- op "::"
  params <- sepBy param (op "->")
  pure { name, params }
  
type Param = 
  { name :: Token
  , args :: Array Token
  }
  
param :: Parse Param
param = try do 
  name <- token TypeName
  args <- many $ token TypeName
  pure { name, args }
  
type Alias = 
  { name :: Token
  , target :: AliasTarget
  }
  
data AliasTarget = AliasedParam Param | AliasedMap TypeMap

-- Parse a type alias
alias :: Parse Alias
alias = try do
  _ <- token Alias
  name <- token TypeName
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
    name <- token FunctionName
    _ <- op "::"
    p <- param
    pure $ name /\ p
    
