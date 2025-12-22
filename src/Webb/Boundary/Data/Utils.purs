module Webb.Boundary.Data.Utils where

import Prelude

import Data.Array as Array
import Data.Foldable (class Foldable)
import Data.Foldable as Fold
import Data.Set as Set



-- Search for duplicates in the data by a derived value. Returns the
-- duplicate data.
findDuplicatesWith :: forall a b f. Ord b => Foldable f => (a -> b) -> f a -> Array a
findDuplicatesWith f list = let 
  init = { seen: Set.empty, dups: [] }
  result = Fold.foldl add_ init list
  in result.dups
  where
  add_ acc@{ seen, dups } item = let 
    b = f item
    in if Set.member b seen then 
      acc { dups = flip Array.snoc item dups }
    else 
      acc { seen = Set.insert b seen }