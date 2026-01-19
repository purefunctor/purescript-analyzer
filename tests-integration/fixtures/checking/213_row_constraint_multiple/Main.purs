module Main where

import Prim.Row as Prim.Row

foreign import unsafeCoerce :: forall a b. a -> b

multiMerge
  :: forall r1 r2 r3 r4 r5 r6
   . Prim.Row.Union r1 r2 r3
  => Prim.Row.Nub r3 r4
  => Prim.Row.Union r4 r5 r6
  => Record r1
  -> Record r5
  -> Record r6
multiMerge _ _ = unsafeCoerce {}

test1 = multiMerge { a: 1 }

test2 = multiMerge { a: 1 } { b: 2 }
