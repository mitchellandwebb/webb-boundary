module Webb.Boundary.Analyzer where

import Prelude

import Data.Either (Either(..))
import Effect.Aff.Class (class MonadAff, liftAff)
import Webb.Boundary.Analyzer.BoundarySymbols as BS
import Webb.Boundary.Analyzer.TypeCheck as TC
import Webb.Boundary.Analyzer.TypeSymbols as TS
import Webb.Boundary.Data.BoundaryTable (BoundaryTable)
import Webb.Boundary.Data.Rep as Rep
import Webb.Boundary.Data.SymbolTable (SymbolTable)
import Webb.Boundary.Data.Tree (Tree)


newtype Analyzer = A Tree

newAnalyzer :: forall m. MonadAff m => Tree -> m Analyzer 
newAnalyzer tree = do
  pure $ A tree
  
buildSymbolTable :: forall m. MonadAff m => 
  Analyzer -> m (Either (Array String) SymbolTable)
buildSymbolTable (A tree) = liftAff do
  TS.buildSymbolTable tree
  
buildBoundaryTable :: forall m. MonadAff m =>
  Analyzer -> SymbolTable -> m (Either (Array String) BoundaryTable)
buildBoundaryTable (A tree) symbols = liftAff do
  BS.buildBoundaryTable tree symbols
  
-- If we pass the type check, we can publish the full representation. But at
-- each step, we might indeed encounter an error.
typeCheck :: forall m. MonadAff m => 
  Analyzer -> SymbolTable -> BoundaryTable -> m (Either (Array String) Rep.Rep)
typeCheck (A tree) symbols bounds = liftAff do
  e <- TC.runTypeCheck symbols tree
  case e of
    Left errors -> pure $ Left errors
    Right _ -> do
      pure $ Right $ Rep.newRep { symbols, bounds, tree }

