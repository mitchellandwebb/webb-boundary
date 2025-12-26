module Webb.Boundary.CompileWriter.CompileWriter where

import Prelude

import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.State (StateT, evalStateT)
import Data.Either (Either(..))
import Data.Foldable as Fold
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Webb.Boundary.Analyzer as Analyzer
import Webb.Boundary.CodeWriter (CodeWriter, newCodeWriter)
import Webb.Boundary.CodeWriter as CodeWriter
import Webb.Boundary.Compile.CompileWriter (CompileWriter)
import Webb.Boundary.Compile.CompileWriter as CW
import Webb.Boundary.Compile.Kotlin (KotlinParams)
import Webb.Boundary.Compile.Kotlin as KP
import Webb.Boundary.Compile.Typescript (TsParams)
import Webb.Boundary.Data.Rep (Rep)
import Webb.Boundary.Data.Rep as Rep
import Webb.Boundary.Data.SymbolTable (SymbolTable)
import Webb.Boundary.Gen.Lang as Lang
import Webb.Boundary.Lang.Kotlin as Kotlin
import Webb.Boundary.Parser as Parser
import Webb.Monad.Prelude (timesRepeat_)
import Webb.State.Prelude (mread)
import Webb.Writer as SWriter


{- The CompileWriter's program knows how to turn various language parameters
  into either an error, or a complete write to some kind of file target.
-}


type State = 
  { writer :: CompileWriter
  }
  
type Prog = ExceptT (Array String) (StateT State Aff)

eval :: forall a. State -> Prog a -> Aff (Either (Array String) a)
eval state prog = prog # runExceptT >>> flip evalStateT state

abort :: forall a. Aff (Either (Array String) a) -> Prog a
abort prog = do
  either <- liftAff prog
  case either of
    Left errors -> do throwError errors
    Right a -> pure a
    
getRep :: Prog Rep
getRep = do
  this <- mread
  let source = CW.source this.writer

  parser <- Parser.newParser
  tokens <- abort $ Parser.tokenize parser source
  tree <- abort $ Parser.parse parser tokens

  analyzer <- Analyzer.newAnalyzer tree
  symbolTable <- abort $ Analyzer.buildSymbolTable analyzer
  boundTable <- abort $ Analyzer.buildBoundaryTable analyzer symbolTable
  rep <- abort $ Analyzer.typeCheck analyzer symbolTable boundTable
  pure rep
  
getSymbolTable :: Prog SymbolTable
getSymbolTable = do
  rep <- getRep
  pure $ Rep.symbolTable rep
    
getCodeWriter :: Lang.Lang_ -> Prog CodeWriter
getCodeWriter lang = do
  this <- mread
  rep <- getRep
  let writer = CW.writer this.writer
  codeWriter <- newCodeWriter rep writer lang
  pure codeWriter
  
writeGeneratedTime :: CodeWriter -> Prog Unit
writeGeneratedTime codeWriter = do
  -- TODO
  pure unit

-- Write the source to file, using kotlin parameters to determine extra
-- things that need to be written.
writeKotlin :: KotlinParams -> Prog Unit
writeKotlin params = do 
  symbolTable <- getSymbolTable
  codeWriter <- getCodeWriter $ Kotlin.kotlinLang symbolTable
  
  writePackage codeWriter
  writeGeneratedTime codeWriter
  CodeWriter.writeAliases codeWriter
  CodeWriter.writeBoundaries codeWriter
  
  where
  writePackage :: CodeWriter -> Prog Unit
  writePackage codeWriter = do
    let package = KP.package params
    CodeWriter.write codeWriter do
      SWriter.words ["package", package]
      timesRepeat_ 2 do SWriter.newline
      
writeTypescript :: TsParams -> Prog Unit
writeTypescript params = do
  symbolTable <- getSymbolTable
  codeWriter <- getCodeWriter $ Typescript.tsLang symbolTable

  writeGeneratedTime codeWriter
  CodeWriter.writeAliases codeWriter
  CodeWriter.writeBoundaries codeWriter
  writeExports codeWriter
  
  where
  writeExports codeWriter = do
    rep <- getRep
    let exports = Rep.allDeclarationNames rep
    CodeWriter.write codeWriter do
      timesRepeat_ 2 do SWriter.newline
      SWriter.words ["export", "{"]
      SWriter.newline
      SWriter.indent 2 do
        Fold.for_ exports \name -> do 
          SWriter.write $ name <> ","
          SWriter.newline
      SWriter.words ["}"]