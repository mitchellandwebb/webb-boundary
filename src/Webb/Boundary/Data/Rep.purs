module Webb.Boundary.Data.Rep where

import Prelude

import Data.Array as Array
import Data.Newtype (class Newtype, unwrap, wrap)
import Webb.Boundary.Data.Alias (Alias)
import Webb.Boundary.Data.Alias as Alias
import Webb.Boundary.Data.Boundary (Boundary)
import Webb.Boundary.Data.Boundary as Bd
import Webb.Boundary.Data.BoundaryTable (BTable)
import Webb.Boundary.Data.BoundaryTable as BTable
import Webb.Boundary.Data.SymbolTable (STable)
import Webb.Boundary.Data.SymbolTable as STable
import Webb.Boundary.Data.Tree (Tree)
import Webb.Monad.Prelude (forceMaybe')
import Webb.Stateful (localEffect)


{- Full representation of the code. -}

newtype Rep = R Rep_

type Rep_ =
  { tree :: Tree
  , symbols :: STable
  , bounds :: BTable
  }
  
derive newtype instance Eq Rep
derive newtype instance Ord Rep
derive newtype instance Show Rep
derive instance Newtype Rep _
  
newRep :: Rep_ -> Rep
newRep = wrap

tree :: Rep -> Tree
tree = unwrap >>> _.tree

symbolTable :: Rep -> STable
symbolTable = unwrap >>> _.symbols

boundTable :: Rep -> BTable
boundTable = unwrap >>> _.bounds

unsortedAliases :: Rep -> Array Alias
unsortedAliases rep = let 
  aliases = STable.aliases $ symbolTable rep
  in aliases

-- Get the boundaries. We can iterate over the data type using the 
-- contained knowledge, to get all the data we need to write the data.
boundaries :: Rep -> Array Boundary
boundaries rep = let
  bounds = BTable.boundaries $ boundTable rep
  in bounds
  
-- Get aliases in sorted order -- fewer dependencies go first.
sortedAliases :: Rep -> Array Alias
sortedAliases rep = let
  symbols = symbolTable rep
  aliases = unsortedAliases rep
  tierList = STable.newTierList symbols
  in Array.sortWith (tier tierList) aliases 
  where
  tier :: STable.AliasTierList -> Alias -> Int
  tier tierList alias = let
    mtier = STable.tier (Alias.name alias) tierList
    notFound = "Alias wasn't found in any tier: " <> show (Alias.name alias)
    in localEffect do 
      forceMaybe' notFound mtier

allDeclarationNames :: Rep -> Array String
allDeclarationNames rep = let
  aliases = sortedAliases rep
  bounds = boundaries rep
  in (Alias.name <$> aliases) <> (Bd.name <$> bounds)