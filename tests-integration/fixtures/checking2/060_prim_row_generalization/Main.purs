module Main where

import Prim.Row as Prim.Row
import Type.Proxy (Proxy(..))

foreign import unsafeCoerce :: forall a b. a -> b

merge
  :: forall r1 r2 r3 r4
   . Prim.Row.Union r1 r2 r3
  => Prim.Row.Nub r3 r4
  => Record r1
  -> Record r2
  -> Record r4
merge _ _ = unsafeCoerce {}

a = merge { a: 123 }

b = a { b: 123 }

fromUnion
  :: forall r1 r2 r3
   . Prim.Row.Union r1 r2 r3
  => Record r3
  -> Int
fromUnion _ = unsafeCoerce 0

test = fromUnion

chainedUnion
  :: forall r1 r2 r3 r4 r5
   . Prim.Row.Union r1 r2 r3
  => Prim.Row.Union r3 r4 r5
  => Record r1
  -> Record r5
chainedUnion _ = unsafeCoerce {}

testChained = chainedUnion { x: 1 }

multiMerge
  :: forall r1 r2 r3 r4 r5 r6
   . Prim.Row.Union r1 r2 r3
  => Prim.Row.Nub r3 r4
  => Prim.Row.Union r4 r5 r6
  => Record r1
  -> Record r5
  -> Record r6
multiMerge _ _ = unsafeCoerce {}

testMulti1 = multiMerge { a: 1 }

testMulti2 = multiMerge { a: 1 } { b: 2 }
