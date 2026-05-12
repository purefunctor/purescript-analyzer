module Main where

import Prim.Int (class Compare)
import Prim.Ordering (EQ, GT, LT)
import Type.Proxy (Proxy(..))

assertLesser :: forall l r. Compare l r LT => Proxy ( left :: l, right :: r )
assertLesser = Proxy

assertGreater :: forall l r. Compare l r GT => Proxy ( left :: l, right :: r )
assertGreater = Proxy

assertEqual :: forall l r. Compare l r EQ => Proxy ( left :: l, right :: r )
assertEqual = Proxy

transLt :: forall m n p. Compare m n LT => Compare n p LT => Proxy n -> Proxy ( left :: m, right :: p )
transLt _ = assertLesser

transLtEq :: forall m n p. Compare m n LT => Compare n p EQ => Proxy n -> Proxy ( left :: m, right :: p )
transLtEq _ = assertLesser

transEqGt :: forall m n p. Compare m n EQ => Compare n p GT => Proxy n -> Proxy ( left :: m, right :: p )
transEqGt _ = assertGreater

transSymmEq :: forall m n p. Compare n m EQ => Compare n p EQ => Proxy n -> Proxy ( left :: m, right :: p )
transSymmEq _ = assertEqual
