module Webb.Boundary.Tree where

import Webb.Boundary.Prelude

import Data.Foldable (for_)
import Data.Map as Map
import Data.Newtype (unwrap)
import Effect.Aff (Aff)
import Webb.Boundary.Parser as P

data Tree 
  = Document (Array Tree)
  | Boundary P.Boundary
  | Alias P.Alias
  
parseTree :: P.Parse Tree
parseTree = do
  trees <- mix [ Boundary <$> P.boundary, Alias <$> P.alias ]
  pure $ Document trees
  
class Visitor a where
  boundary :: a -> P.Boundary -> Aff Unit
  method :: a -> P.Method -> Aff Unit
  param :: a -> P.Param -> Aff Unit
  alias :: a -> P.Alias -> Aff Unit
  typeMap :: a -> P.TypeMap -> Aff Unit

defaultBoundary :: forall a. a -> P.Boundary -> Aff Unit
defaultBoundary _ _ = pure unit

defaultMethod :: forall a. a -> P.Method -> Aff Unit
defaultMethod _ _ = pure unit

defaultParam :: forall a. a -> P.Param -> Aff Unit
defaultParam _ _ = pure unit

defaultAlias :: forall a. a -> P.Alias -> Aff Unit
defaultAlias _ _ = pure unit

defaultTypeMap :: forall a. a -> P.TypeMap -> Aff Unit
defaultTypeMap _ _ = pure unit
  
visit :: forall a. Visitor a => a -> Tree -> Aff Unit
visit visitor tree = case tree of
  Document arr -> do
    for_ arr \statement -> visit visitor statement
  Boundary t -> do
    boundary visitor t
    for_ t.methods visitMethod
  Alias t -> do
    alias visitor t
    case t.target of 
      P.AliasedParam p -> do
        visitParam p
        let uw = unwrap p
        for_ uw.args visitParam
      P.AliasedMap m -> do
        visitMap m
        
  where
  visitMethod t = do
    method visitor t
    for_ t.params visitParam
    
  visitParam t = do
    param visitor t
    
  visitMap t = do 
    typeMap visitor t
    let values = Map.values t
    for_ values visitParam
        
newtype GeneralVisitor = GV GV_

type GV_ = 
  { boundary :: P.Boundary -> Aff Unit
  , method :: P.Method -> Aff Unit
  , param :: P.Param -> Aff Unit
  , alias :: P.Alias -> Aff Unit
  , typeMap :: P.TypeMap -> Aff Unit
  }

default :: GV_
default =
  { boundary: \_ -> pure unit
  , method : \_ -> pure unit
  , param : \_ -> pure unit
  , alias: \_ -> pure unit
  , typeMap : \_ -> pure unit
  }

instance Visitor GeneralVisitor where
  boundary (GV s) = s.boundary
  method (GV s) = s.method
  param (GV s) = s.param
  alias (GV s) = s.alias
  typeMap (GV s) = s.typeMap
  
allBoundaries :: Tree -> (P.Boundary -> Aff Unit) -> Aff Unit
allBoundaries tree prog = do
  let v = GV $ default { boundary = prog }
  visit v tree

allMethods :: Tree -> (P.Method -> Aff Unit) -> Aff Unit
allMethods tree prog = do
  let v = GV $ default { method = prog }
  visit v tree

allParams :: Tree -> (P.Param -> Aff Unit) -> Aff Unit
allParams tree prog = do
  let v = GV $ default { param = prog }
  visit v tree

allAliases :: Tree -> (P.Alias -> Aff Unit) -> Aff Unit
allAliases tree prog = do
  let v = GV $ default { alias = prog }
  visit v tree

allTypeMaps :: Tree -> (P.TypeMap -> Aff Unit) -> Aff Unit
allTypeMaps tree prog = do
  let v = GV $ default { typeMap = prog }
  visit v tree
