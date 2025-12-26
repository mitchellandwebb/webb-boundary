module Webb.Boundary.CodeWriter where

import Prelude

import Data.Foldable as Fold
import Data.Newtype (class Newtype, unwrap)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Webb.Boundary.Data.Rep (Rep)
import Webb.Boundary.Data.Rep as Rep
import Webb.Boundary.Gen as Gen
import Webb.Boundary.Gen.Lang (Lang_)
import Webb.Monad.Prelude (timesRepeat_)
import Webb.Writer (SWriter)
import Webb.Writer as SWriter


{- Writing code based on the representation, writing to a single file.
-}

newtype CodeWriter = C CodeWriter_

type CodeWriter_ = 
  { rep :: Rep
  , writeToFile :: String -> Aff Unit
  , lang :: Lang_
  }
  
derive instance Newtype CodeWriter _
  
newCodeWriter :: forall m. MonadAff m => 
  Rep -> (String -> Aff Unit) -> Lang_ -> m CodeWriter
newCodeWriter rep writeToFile lang = do
  pure $ C { rep, writeToFile, lang }
  
-- Write arbitrary code to the file. Useful for language-specific code
-- that isn't dependent on the code representation.
write :: forall m. MonadAff m => 
  CodeWriter -> SWriter Unit -> m Unit
write cw prog = liftAff do
  let s = unwrap cw
      code = SWriter.runToString prog
  s.writeToFile code

writeString :: forall m. MonadAff m => 
  CodeWriter -> String -> m Unit
writeString cw str = liftAff do
  let s = unwrap cw
  s.writeToFile str
  
-- Write the aliases to file.
writeAliases :: forall m. MonadAff m =>
  CodeWriter -> m Unit
writeAliases cw = do
  let 
    s = unwrap cw
    aliases = Rep.sortedAliases s.rep

  Fold.for_ aliases \alias -> do
    let code = Gen.fromAlias s.lang alias
    write cw do
      SWriter.write code
      timesRepeat_ 2 do SWriter.newline
  
-- Write boundaries to file.
writeBoundaries :: forall m. MonadAff m =>
  CodeWriter -> m Unit
writeBoundaries cw = do
  let 
    s = unwrap cw
    bounds = Rep.boundaries s.rep
    
  Fold.for_ bounds \bound -> do
    let code = Gen.fromBoundary s.lang bound
    write cw do
      SWriter.write code
      timesRepeat_ 2 do SWriter.newline
  