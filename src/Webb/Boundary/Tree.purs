module Webb.Boundary.Tree where

import Webb.Boundary.Prelude

import Data.Foldable (for_)
import Data.Map as Map
import Data.Newtype (unwrap)
import Effect.Aff (Aff)
import Webb.Boundary.Parser as P

data Tree 
  = Statements (Array Tree)
  | Boundary P.Boundary
  | Alias P.Alias
  
class Visitor a where
  boundary :: a -> P.Boundary -> Aff Unit
  method :: a -> P.Method -> Aff Unit
  param :: a -> P.Param -> Aff Unit
  alias :: a -> P.Alias -> Aff Unit
  typeMap :: a -> P.TypeMap -> Aff Unit
  
visit :: forall a. Visitor a => a -> Tree -> Aff Unit
visit visitor tree = case tree of
  Statements arr -> do
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
        


