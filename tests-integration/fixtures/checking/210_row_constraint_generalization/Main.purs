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

-- This should generalise the unsolved Union and Nub constraints
-- rather than emitting AmbiguousConstraint errors
a = merge { a: 123 }

-- Fully applied, should resolve to a concrete record type
b = a { b: 123 }
