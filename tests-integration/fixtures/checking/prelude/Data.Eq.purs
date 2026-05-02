module Data.Eq where

import Data.Symbol (class IsSymbol)
import Prim.Row as Row
import Prim.RowList as RL

class Eq a where
  eq :: a -> a -> Boolean

class Eq1 f where
  eq1 :: forall a. Eq a => f a -> f a -> Boolean

instance Eq Int where
  eq _ _ = true

instance Eq Boolean where
  eq _ _ = true

instance eqRec :: (RL.RowToList row list, EqRecord list row) => Eq (Record row) where
  eq _ _ = true

class EqRecord :: RL.RowList Type -> Row Type -> Constraint
class EqRecord rowlist row where
  eqRecord :: rowlist -> Record row -> Record row -> Boolean

instance EqRecord RL.Nil row where
  eqRecord _ _ _ = true

instance
  ( EqRecord rowlistTail row
  , Row.Cons key focus rowTail row
  , IsSymbol key
  , Eq focus
  ) =>
  EqRecord (RL.Cons key focus rowlistTail) row where
  eqRecord _ _ _ = true
