module Webb.Boundary.Data.Token where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Show.Generic (genericShow)


newtype Token = TK Token_

type Token_ = 
  { string :: String
  , kind :: TokenKind
  , line :: Int
  , column :: Int
  , endLine :: Int
  , endColumn :: Int
  , index :: Int
  }

type Position = 
  { line :: Int
  , column :: Int
  , endLine :: Int
  , endColumn :: Int
  , index :: Int
  }
  
derive newtype instance Eq Token
derive newtype instance Ord Token
derive newtype instance Show Token
derive instance Newtype Token _
  
data TokenKind 
  = Operator 
  | Boundary
  | Where
  | Alias
  | TypeName
  | FunctionName
  | Delim
  | Separator
  
derive instance Eq TokenKind
derive instance Ord TokenKind
derive instance Generic TokenKind _
instance Show TokenKind where show = genericShow

new :: Token_ -> Token
new = wrap

kind :: Token -> TokenKind
kind = unwrap >>> _.kind

hasText :: String -> Token -> Boolean
hasText s t = (text t == s)

text :: Token -> String
text = unwrap >>> _.string

position :: Token -> Position
position tok = let
  s = unwrap tok
  in 
  { line: s.line
  , column: s.column
  , endLine: s.line
  , endColumn: s.column
  , index: s.index
  }