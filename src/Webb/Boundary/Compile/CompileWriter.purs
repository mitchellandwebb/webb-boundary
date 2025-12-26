module Webb.Boundary.Compile.CompileWriter where

import Prelude

import Data.Newtype (class Newtype, unwrap, wrap)
import Effect.Aff (Aff)


{- Represents the ability to write a source string to a file. -}


newtype CompileWriter = FW
  { source :: String -- source code of the file
  , write :: String -> Aff Unit -- Ability to write to the file.
  }
  
type CWriter = CompileWriter
  
derive instance Newtype CompileWriter _

newCompileWriter :: String -> (String -> Aff Unit) -> CompileWriter
newCompileWriter source' write' = wrap { source: source', write: write' }
  
source :: CompileWriter -> String
source = unwrap >>> _.source

write :: CompileWriter -> String -> Aff Unit
write fw str = do 
  let s = unwrap fw
  s.write str

writer :: CompileWriter -> (String -> Aff Unit)
writer fw = write fw