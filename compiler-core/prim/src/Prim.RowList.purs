module Prim.RowList where

data RowList :: Type -> Type
data RowList kind

data Cons :: forall (k :: Type). Symbol -> k -> RowList k -> RowList k
data Cons label head tail

data Nil :: forall (k :: Type). RowList k
data Nil

class RowList :: forall (k :: Type). Row k -> RowList k
class RowList row list | row -> list
