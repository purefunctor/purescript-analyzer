module Main where

import Prim.Row as Row
import Type.Proxy (Proxy(..))

foreign import unsafeCoerce :: forall a b. a -> b

class Row.Union r1 r2 r3 <= Merge r1 r2 r3

useMerge
  :: forall r1 r2 r3
   . Merge r1 r2 r3
  => Proxy r3
  -> Record r1
  -> Record r2
  -> Int
useMerge _ _ _ = unsafeCoerce 0

useUnion
  :: forall r1 r2 r3
   . Row.Union r1 r2 r3
  => Proxy r3
  -> Record r1
  -> Record r2
  -> Int
useUnion _ _ _ = unsafeCoerce 0

testMin p r1 r2 =
  let
    _ = useMerge p r1 r2
  in
    useUnion p r1 r2
