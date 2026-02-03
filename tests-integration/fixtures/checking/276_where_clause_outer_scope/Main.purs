module Main where

import Type.Proxy (Proxy(..))

-- Minimal reproduction: where clause referencing outer type variables
-- The `r` in coerce's signature should reference the outer forall's `r`

foreign import unsafeCoerce :: forall a b. a -> b

test :: forall t r. t -> (forall v. Proxy v -> r) -> r
test s f = coerce f Proxy
  where
  coerce
    :: (forall v. Proxy v -> r)
    -> Proxy _
    -> r
  coerce = unsafeCoerce
