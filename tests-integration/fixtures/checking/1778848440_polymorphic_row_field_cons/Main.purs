module Main where

import Prim.Row as Row

data Proxy :: forall k. k -> Type
data Proxy a = Proxy

data Slot :: (Type -> Type) -> Type -> Type -> Type
data Slot query output key

data Html :: Row Type -> Type
data Html slots = Html

type Slots key =
  ( child :: forall query output. Slot query output key
  )

_child :: Proxy "child"
_child = Proxy

slot
  :: forall query output slots label key tail
   . Row.Cons label (Slot query output key) tail slots
  => Proxy label
  -> key
  -> Html slots
slot _ _ = Html

test :: forall key. key -> Html (Slots key)
test key = slot _child key
