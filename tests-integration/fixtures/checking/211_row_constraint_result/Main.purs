module Main where

import Prim.Row as Prim.Row

foreign import unsafeCoerce :: forall a b. a -> b

fromUnion
  :: forall r1 r2 r3
   . Prim.Row.Union r1 r2 r3
  => Record r3
  -> Int
fromUnion _ = unsafeCoerce 0

test = fromUnion
