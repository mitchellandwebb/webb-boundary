module Webb.Boundary.Parser.Parser where

import Webb.Boundary.Prelude

import Data.Either (Either(..))
import Data.List (List)
import Data.List as List
import Data.Map as Map
import Effect.Aff (Aff)
import Parsing as P
import Parsing.Token as T
import Webb.Boundary.Data.Alias (Alias)
import Webb.Boundary.Data.Alias as Alias
import Webb.Boundary.Data.Boundary (Boundary)
import Webb.Boundary.Data.Boundary as Bound
import Webb.Boundary.Data.Method (Method)
import Webb.Boundary.Data.Method as Method
import Webb.Boundary.Data.Param (Param)
import Webb.Boundary.Data.Param as Param
import Webb.Boundary.Data.Token (Token, TokenKind)
import Webb.Boundary.Data.Token as Tok
import Webb.Boundary.Data.TypeMap (TypeMap)
import Webb.Boundary.Data.TypeMap as TypeMap
import Webb.Boundary.Data.Tree as Tree

type Parse = Parser (List Token)

treeParser :: Parse Tree.Tree
treeParser = do
  trees <- mix [ Tree.ABoundary <$> boundary, Tree.AnAlias <$> alias ]
  pure $ Tree.Document trees

parseTokens :: forall a. Array Token -> Parse a -> Aff (Either String a)
parseTokens tokens prog = do
  let stream = List.fromFoldable tokens
  case P.runParser stream prog of
    Left (P.ParseError msg pos) -> do
      let msg' = show pos <> " " <> msg
      pure $ Left msg'
    Right result -> do
      pure $ Right result

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
    
-- Parse a boundary definition.
boundary :: Parse Boundary
boundary = try do 
  _ <- token Tok.Boundary 
  name <- token Tok.TypeName
  _ <- token Tok.Where
  methods <- many method
  
  pure $ Bound.newBoundary
    { name: name
    , methods
    }
    
method :: Parse Method
method = try do
  name <- token Tok.FunctionName
  _ <- op "::"
  params <- sepBy param (op "->")
  pure $ Method.newMethod { name, params }
  
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

-- Parse a type alias
alias :: Parse Alias
alias = try do
  _ <- token Tok.Alias
  name <- token Tok.TypeName
  _ <- op "="
  target <- alts [ Alias.AliasedParam <$> param, Alias.AliasedMap <$> typeMap ]
  pure $ Alias.newAlias { name, target }
  
-- Parse a type map -- in other words, a Record type
typeMap :: Parse TypeMap
typeMap = try do 
  _ <- delim "{"
  pairs <- sepEndBy pair (sep ",")
  _ <- delim "}"
  let map = Map.fromFoldable pairs
  pure $ TypeMap.newTypeMap map

  where
  pair = try do
    name <- token Tok.FunctionName
    _ <- op "::"
    p <- param
    pure $ name /\ p
    
