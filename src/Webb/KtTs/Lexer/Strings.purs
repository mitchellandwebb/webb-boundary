module Webb.KtTs.Lexer.Strings where

import Webb.KtTs.Prelude

import Data.Array as A
import Data.String as Str
import Parsing.String (string)


{- Basic strings that we will combine into tokens parse. -}

type Parse = Parser String String

s :: String -> Parse 
s str = string str

dot :: Parse 
dot = s "."

import' :: Parse 
import' = s "import"

val :: Parse 
val = s "val"

var :: Parse 
var = s "var"

clazz :: Parse 
clazz = s "class"

fun :: Parse 
fun = s "fun"

companion :: Parse 
companion = s "companion"

object :: Parse 
object = s "object"

package :: Parse 
package = s "package"

-- The '*' character. Useful in specified a package import like package1.*
star :: Parse
star = s "*"

operator :: Parse
operator = do
  strings <- mix1 $ pure <$>
    [ "*"
    , "+"
    , "-"
    , "="
    , "$"
    , "#"
    ]
  pure $ Str.joinWith "" (A.fromFoldable strings)

-- An identifier, in simplified form, is any mix of alphanumeric characters 
-- and underscores. Can refer to packages, type names, and variable names.
ident :: Parse
ident = do 
  strings <- mix1 [alphaChar, numChar, s "_"]
  pure $ Str.joinWith "" $ A.fromFoldable strings
