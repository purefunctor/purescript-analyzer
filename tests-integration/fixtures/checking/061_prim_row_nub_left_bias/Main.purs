module Main where

import Prim.Row as Prim.Row

foreign import unsafeCoerce :: forall a b. a -> b

merge
  :: forall r1 r2 r3 r4
   . Prim.Row.Union r1 r2 r3
  => Prim.Row.Nub r3 r4
  => Record r1
  -> Record r2
  -> Record r4
merge _ _ = unsafeCoerce {}

test = merge { a: 42, b: "life" } { b: 42 }
