module Webb.KtTs.Lexer.Token where

import Prelude
import Webb.KtTs.Prelude

import Webb.KtTs.Lexer.Strings as S



type Token = 
  { kind :: TokenKind
  , string :: String
  , line :: Int
  , column :: Int
  , endLine :: Int
  , endColumn :: Int
  }
  

-- We label tokens so that we can tokenize them easily, and to ensure that
-- we looked at the longest strings while parsing. If we didn't tokenize, we'd
-- just be looking at strings, and wouldn't know what we had recognized.

data TokenKind 
  = Id
  | Package 
  | Class
  | Interface 
  | Alias
  | Dot
  | PackageName
  | ImportTarget
  | Object
  | Companion
  | Val  
  | Var
  | Colon
  | Semicolon
  
token :: Parser String Token
token = longest $ asToken <$>
  [ Id /\ S.ident
  , Package /\ S.package 
  , Class /\ S.clazz
  , Interface /\ S.Interface
  ]
