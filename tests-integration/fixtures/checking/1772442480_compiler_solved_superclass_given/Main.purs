module Main where

import Prim.Row as Row

foreign import unsafeCoerce :: forall a b. a -> b

class Row.Union r1 r2 r3 <= Merge r1 r2 r3

merge
  :: forall r1 r2 r3
   . Row.Union r1 r2 r3
  => Record r1
  -> Record r2
  -> Record r3
merge _ _ = unsafeCoerce {}

merge2
  :: forall r1 r2 r3
   . Merge r1 r2 r3
  => Record r1
  -> Record r2
  -> Record r3
merge2 r1 r2 = merge r1 r2
