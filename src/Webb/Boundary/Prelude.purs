module Webb.Boundary.Prelude
( module P
, s
, sepBy, sepBy1, sepEndBy, sepEndBy1
, joined
) where

import Prelude

import Data.Array as A
import Data.String (joinWith)
import Data.Tuple (uncurry) as P
import Data.Tuple.Nested ((/\), type (/\)) as P
import Parsing (ParserT)
import Parsing as P
import Parsing.Combinators (try, option, optionMaybe, optional) as P
import Parsing.Combinators as C
import Parsing.Combinators.Array as P
import Parsing.String (string)
import Prelude as P
import Prelude as P
import Webb.Parsing as P
import Webb.Parsing.Combinators as P
import Webb.Parsing.String as P
import Webb.State.Prelude as P

sepBy :: forall m s a sep. ParserT s m a -> ParserT s m sep -> ParserT s m (Array a)
sepBy p sep = do 
  list <- C.sepBy p sep
  pure $ A.fromFoldable list

sepBy1 :: forall m s a sep. 
  ParserT s m a -> ParserT s m sep -> ParserT s m (Array a)
sepBy1 p sep = do 
  list <- C.sepBy1 p sep
  pure $ A.fromFoldable list

sepEndBy :: forall m s a sep. 
  ParserT s m a -> ParserT s m sep -> ParserT s m (Array a)
sepEndBy p sep = do 
  list <- C.sepEndBy p sep
  pure $ A.fromFoldable list

sepEndBy1 :: forall m s a sep. 
  ParserT s m a -> ParserT s m sep -> ParserT s m (Array a)
sepEndBy1 p sep = do 
  list <- C.sepEndBy1 p sep
  pure $ A.fromFoldable list

s :: forall m. String -> ParserT String m String
s str = string str

joined :: forall m s. Array String -> ParserT s m String
joined strs = C.try do
  pure $ joinWith "" strs