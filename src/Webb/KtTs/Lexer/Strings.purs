module Webb.KtTs.Lexer.Strings where

import Webb.KtTs.Prelude

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

star :: Parse
star = s "*"

ident :: Parse
ident = mix [alpha, numeric, underscore]