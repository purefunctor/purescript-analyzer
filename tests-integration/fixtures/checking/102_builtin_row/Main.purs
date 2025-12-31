module Main where

import Prim.Row as Row
import Prim.RowList as RL

data Proxy :: forall k. k -> Type
data Proxy a = Proxy

deriveUnion :: forall u. Row.Union (a :: Int) (b :: String) u => Proxy u
deriveUnion = Proxy

deriveUnionLeft :: forall l. Row.Union l (b :: String) (a :: Int, b :: String) => Proxy l
deriveUnionLeft = Proxy

deriveUnionRight :: forall r. Row.Union (a :: Int) r (a :: Int, b :: String) => Proxy r
deriveUnionRight = Proxy

unionEmptyLeft :: forall u. Row.Union () (a :: Int) u => Proxy u
unionEmptyLeft = Proxy

unionEmptyRight :: forall u. Row.Union (a :: Int) () u => Proxy u
unionEmptyRight = Proxy

unionBothEmpty :: forall u. Row.Union () () u => Proxy u
unionBothEmpty = Proxy

unionMultiple :: forall u. Row.Union (a :: Int, b :: String) (c :: Boolean) u => Proxy u
unionMultiple = Proxy

deriveCons :: forall row. Row.Cons "name" String () row => Proxy row
deriveCons = Proxy

deriveTail :: forall tail. Row.Cons "name" String tail (name :: String, age :: Int) => Proxy tail
deriveTail = Proxy

deriveType :: forall t. Row.Cons "name" t () (name :: String) => Proxy t
deriveType = Proxy

nestedCons :: forall row. Row.Cons "a" Int (b :: String) row => Proxy row
nestedCons = Proxy

lacksSimple :: forall r. Row.Lacks "missing" (a :: Int, b :: String) => Proxy r -> Proxy r
lacksSimple = \x -> x

lacksEmpty :: forall r. Row.Lacks "anything" () => Proxy r -> Proxy r
lacksEmpty = \x -> x

nubNoDuplicates :: forall nubbed. Row.Nub (a :: Int, b :: String) nubbed => Proxy nubbed
nubNoDuplicates = Proxy

nubEmpty :: forall nubbed. Row.Nub () nubbed => Proxy nubbed
nubEmpty = Proxy

rowToListSimple :: forall list. RL.RowToList (a :: Int) list => Proxy list
rowToListSimple = Proxy

rowToListMultiple :: forall list. RL.RowToList (b :: String, a :: Int) list => Proxy list
rowToListMultiple = Proxy

rowToListEmpty :: forall list. RL.RowToList () list => Proxy list
rowToListEmpty = Proxy

rowToListThree :: forall list. RL.RowToList (c :: Boolean, a :: Int, b :: String) list => Proxy list
rowToListThree = Proxy

solveUnion =
  { deriveUnion
  , deriveUnionLeft
  , deriveUnionRight
  , unionEmptyLeft
  , unionEmptyRight
  , unionBothEmpty
  , unionMultiple
  }

solveCons =
  { deriveCons
  , deriveTail
  , deriveType
  , nestedCons
  }

solveLacks =
  { lacksSimple: lacksSimple Proxy
  , lacksEmpty: lacksEmpty Proxy
  }

solveNub =
  { nubNoDuplicates
  , nubEmpty
  }

solveRowToList =
  { rowToListSimple
  , rowToListMultiple
  , rowToListEmpty
  , rowToListThree
  }
