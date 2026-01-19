module Main where

import Prim.Row as Prim.Row

foreign import unsafeCoerce :: forall a b. a -> b

chainedUnion
  :: forall r1 r2 r3 r4 r5
   . Prim.Row.Union r1 r2 r3
  => Prim.Row.Union r3 r4 r5
  => Record r1
  -> Record r5
chainedUnion _ = unsafeCoerce {}

test = chainedUnion { x: 1 }
