module Main where

import Prim.RowList (class RowToList, RowList(..))
import Type.Proxy (Proxy(..))

data Unit = Unit

-- Class with two instances: one with RowToList context, one without
class MatchRow :: Type -> Type -> Constraint
class MatchRow key row | key -> row where
  matchRow :: Proxy key -> Proxy row -> Unit

-- Instance without context: matches everything but is less specific
instance matchRowOther :: MatchRow Unit a where
  matchRow _ _ = Unit

-- Instance with RowToList context: more specific, should be selected
-- Uses Record type constructor with open row pattern
else instance matchRowRowList :: RowToList r rl => MatchRow Unit (Record r) where
  matchRow _ _ = Unit

-- Test: Record with closed row should match the else instance
test :: MatchRow Unit (Record (a :: Int, b :: String)) => Proxy Unit
test = Proxy
