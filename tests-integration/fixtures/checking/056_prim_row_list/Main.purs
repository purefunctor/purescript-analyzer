module Main where

import Prim.RowList as RL
import Type.Proxy (Proxy(..))

rowToListSimple :: forall list. RL.RowToList (a :: Int) list => Proxy list
rowToListSimple = Proxy

rowToListMultiple :: forall list. RL.RowToList (b :: String, a :: Int) list => Proxy list
rowToListMultiple = Proxy

rowToListEmpty :: forall list. RL.RowToList () list => Proxy list
rowToListEmpty = Proxy

rowToListThree :: forall list. RL.RowToList (c :: Boolean, a :: Int, b :: String) list => Proxy list
rowToListThree = Proxy

stuckOpenRow :: forall tail list. RL.RowToList (a :: Int | tail) list => Proxy tail -> Proxy list
stuckOpenRow _ = Proxy

forceSolve =
  { rowToListSimple
  , rowToListMultiple
  , rowToListEmpty
  , rowToListThree
  , nowSolved: stuckOpenRow (Proxy :: Proxy ())
  }
