module Main where

import Prim.Row as Row
import Type.Proxy (Proxy(..))

unionTypeMismatch :: forall l. Row.Union l (b :: String) (a :: Int, b :: Int) => Proxy l
unionTypeMismatch = Proxy

consMissingLabel :: forall t. Row.Cons "missing" Int t (a :: Int, b :: String) => Proxy t
consMissingLabel = Proxy

lacksPresent :: Row.Lacks "b" (a :: Int, b :: String) => Proxy (a :: Int, b :: String)
lacksPresent = Proxy

forceSolve =
  { unionTypeMismatch
  , consMissingLabel
  , lacksPresent
  }
