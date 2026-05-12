module Main where

import Prim.Int (class Compare)
import Prim.Ordering (GT, LT)
import Type.Proxy (Proxy(..))

assertGreater :: forall l r. Compare l r GT => Proxy ( left :: l, right :: r )
assertGreater = Proxy

assertLesser :: forall l r. Compare l r LT => Proxy ( left :: l, right :: r )
assertLesser = Proxy

-- Given h > 0, derive h > -1 by transitivity through the literals 0 and -1.
weakenZeroToNegOne :: forall h. Compare h 0 GT => Proxy ( left :: h, right :: (-1) )
weakenZeroToNegOne = assertGreater

-- Given h > 0, derive h > -2 (chain through two concrete literals).
weakenZeroToNegTwo :: forall h. Compare h 0 GT => Proxy ( left :: h, right :: (-2) )
weakenZeroToNegTwo = assertGreater

-- Given h < 0, derive h < 1.
weakenZeroLT :: forall h. Compare h 0 LT => Proxy ( left :: h, right :: 1 )
weakenZeroLT = assertLesser

-- Mix abstract chain with concrete seeding: h > k and k > 5 implies h > 4.
chainAbstractAndConcrete
  :: forall h k
   . Compare h k GT
  => Compare k 5 GT
  => Proxy ( left :: h, right :: 4 )
chainAbstractAndConcrete = assertGreater
