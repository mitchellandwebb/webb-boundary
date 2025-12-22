module Webb.Boundary.Tree where

import Webb.Boundary.Prelude

import Data.Foldable (for_)
import Data.Newtype (unwrap)
import Effect.Aff (Aff)
import Webb.Boundary.Data.Boundary (Boundary)
import Webb.Boundary.Data.Boundary as Bound
import Webb.Boundary.Data.Method (Method)
import Webb.Boundary.Data.Method as Method
import Webb.Boundary.Parser as P
import Webb.Boundary.Data.Param (Param)
import Webb.Boundary.Data.Alias (Alias)
import Webb.Boundary.Data.Alias as Alias
import Webb.Boundary.Data.TypeMap (TypeMap)
import Webb.Boundary.Data.TypeMap as TypeMap

data Tree 
  = Document (Array Tree)
  | ABoundary Boundary
  | AnAlias Alias
  
parseTree :: P.Parse Tree
parseTree = do
  trees <- mix [ ABoundary <$> P.boundary, AnAlias <$> P.alias ]
  pure $ Document trees
  
class Visitor a where
  boundary :: a -> Boundary -> Aff Unit
  method :: a -> Method -> Aff Unit
  param :: a -> Param -> Aff Unit
  alias :: a -> Alias -> Aff Unit
  typeMap :: a -> TypeMap -> Aff Unit

defaultBoundary :: forall a. a -> Boundary -> Aff Unit
defaultBoundary _ _ = pure unit

defaultMethod :: forall a. a -> Method -> Aff Unit
defaultMethod _ _ = pure unit

defaultParam :: forall a. a -> Param -> Aff Unit
defaultParam _ _ = pure unit

defaultAlias :: forall a. a -> Alias -> Aff Unit
defaultAlias _ _ = pure unit

defaultTypeMap :: forall a. a -> TypeMap -> Aff Unit
defaultTypeMap _ _ = pure unit
  
visit :: forall a. Visitor a => a -> Tree -> Aff Unit
visit visitor tree = case tree of
  Document arr -> do
    for_ arr \statement -> visit visitor statement
  ABoundary t -> do
    boundary visitor t
    for_ (Bound.methods t) visitMethod
  AnAlias t -> do
    alias visitor t
    case Alias.target t of 
      Alias.AliasedParam p -> do
        visitParam p
        let uw = unwrap p
        for_ uw.args visitParam
      Alias.AliasedMap m -> do
        visitMap m
        
  where
  visitMethod t = do
    method visitor t
    for_ (Method.params t) visitParam
    
  visitParam t = do
    param visitor t
    
  visitMap t = do 
    typeMap visitor t
    let params = TypeMap.params t
    for_ params visitParam
        
newtype GeneralVisitor = GV GV_

type GV_ = 
  { boundary :: Boundary -> Aff Unit
  , method :: Method -> Aff Unit
  , param :: Param -> Aff Unit
  , alias :: Alias -> Aff Unit
  , typeMap :: TypeMap -> Aff Unit
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
  
allBoundaries :: Tree -> (Boundary -> Aff Unit) -> Aff Unit
allBoundaries tree prog = do
  let v = GV $ default { boundary = prog }
  visit v tree

allMethods :: Tree -> (Method -> Aff Unit) -> Aff Unit
allMethods tree prog = do
  let v = GV $ default { method = prog }
  visit v tree

allParams :: Tree -> (Param -> Aff Unit) -> Aff Unit
allParams tree prog = do
  let v = GV $ default { param = prog }
  visit v tree

allAliases :: Tree -> (Alias -> Aff Unit) -> Aff Unit
allAliases tree prog = do
  let v = GV $ default { alias = prog }
  visit v tree

allTypeMaps :: Tree -> (TypeMap -> Aff Unit) -> Aff Unit
allTypeMaps tree prog = do
  let v = GV $ default { typeMap = prog }
  visit v tree
