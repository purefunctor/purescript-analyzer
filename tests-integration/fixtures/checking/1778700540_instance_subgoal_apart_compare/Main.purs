module Main where

import Prim.Int (class Compare)
import Prim.Ordering (GT, LT)
import Type.Proxy (Proxy(..))

class Pick :: Int -> Type -> Constraint
class Pick index result | index -> result

instance Compare index 1 LT => Pick index String
else instance Compare index 1 GT => Pick index Int

pick :: forall index result. Pick index result => Proxy index -> Proxy result
pick _ = Proxy

test :: Proxy Int
test = pick (Proxy :: Proxy 2)

testInferred = pick (Proxy :: Proxy 2)
