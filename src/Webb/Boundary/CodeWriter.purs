module Webb.Boundary.CodeWriter where

import Prelude

import Data.Foldable as Fold
import Data.Newtype (class Newtype, unwrap)
import Effect.Aff.Class (class MonadAff)
import Webb.Boundary.Data.Rep (Rep)
import Webb.Boundary.Data.Rep as Rep
import Webb.Boundary.Gen as Gen
import Webb.Boundary.Gen.Lang (Lang_)
import Webb.File (File)
import Webb.File as File
import Webb.Monad.Prelude (timesRepeat_)
import Webb.Writer (SWriter)
import Webb.Writer as SWriter


{- Writing code based on the representation, writing to a single file.
-}

newtype CodeWriter = C CodeWriter_

type CodeWriter_ = 
  { rep :: Rep
  , file :: File
  , lang :: Lang_
  }
  
derive instance Newtype CodeWriter _
  
newCodeWriter :: forall m. MonadAff m => 
  Rep -> File -> Lang_ -> m CodeWriter
newCodeWriter rep file lang = do
  pure $ C { rep, file, lang }
  
open :: forall m. MonadAff m => 
  CodeWriter -> m Unit
open cw = do
  let s = unwrap cw
  File.openTruncated s.file
  
close :: forall m. MonadAff m => 
  CodeWriter -> m Unit
close cw = do
  let s = unwrap cw
  File.close s.file
  
-- Write arbitrary code to the file. Useful for language-specific code
-- that isn't dependent on the code representation.
write :: forall m. MonadAff m => 
  CodeWriter -> SWriter Unit -> m Unit
write cw prog = do
  let s = unwrap cw
      code = SWriter.runToString prog
  File.writeString s.file code

writeString :: forall m. MonadAff m => 
  CodeWriter -> String -> m Unit
writeString cw str = do
  let s = unwrap cw
  File.writeString s.file str
  
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
  
