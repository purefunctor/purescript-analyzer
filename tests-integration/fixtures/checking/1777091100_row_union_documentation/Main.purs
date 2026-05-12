module Main where

import Prim.Row as Row

data Proxy :: forall k. k -> Type
data Proxy a = Proxy

closedLeftOpenRight :: forall r u. Row.Union (a :: Int) (b :: String | r) u => Proxy u
closedLeftOpenRight = Proxy

closedLeftKnownFields :: forall u. Row.Union (a :: Int) (b :: String) u => Proxy u
closedLeftKnownFields = Proxy

closedLeftEmpty :: forall r u. Row.Union () (b :: String | r) u => Proxy u
closedLeftEmpty = Proxy

closedRightOutputOpenLeft :: forall l. Row.Union l (b :: String) (a :: Int, b :: String) => Proxy l
closedRightOutputOpenLeft = Proxy

closedRightOutputKnownLeft :: forall l. Row.Union (a :: Int | l) (b :: String) (a :: Int, b :: String, c :: Boolean) => Proxy l
closedRightOutputKnownLeft = Proxy

closedRightOutputDuplicate :: forall l. Row.Union l (x :: String) (x :: Int, x :: String) => Proxy l
closedRightOutputDuplicate = Proxy

openLeftPrefixOne :: forall rest r u. Row.Union (a :: Int | rest) (b :: String | r) u => Proxy u
openLeftPrefixOne = Proxy

openLeftPrefixMany :: forall rest u. Row.Union (a :: Int, b :: String | rest) (c :: Boolean) u => Proxy u
openLeftPrefixMany = Proxy

forceSolve =
  { closedLeftOpenRight
  , closedLeftKnownFields
  , closedLeftEmpty
  , closedRightOutputOpenLeft
  , closedRightOutputKnownLeft
  , closedRightOutputDuplicate
  , openLeftPrefixOne
  , openLeftPrefixMany
  }
