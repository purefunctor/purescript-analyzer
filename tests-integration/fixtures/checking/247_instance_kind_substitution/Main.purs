module Main where

import Prim.RowList (class RowToList)

data Proxy :: forall k. k -> Type
data Proxy a = Proxy

class Use :: forall k. k -> Constraint
class Use a where
  use :: Proxy a

instance RowToList (x :: a) xs => Use a where
  use = Proxy

test :: Proxy String
test = use
