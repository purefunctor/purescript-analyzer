module Main where

import Prim.Int (class Add)
import Type.Proxy (Proxy(..))

class Program n m

instance (Add n 1 n1, Add n1 1 n2) => Program n n2

add :: forall n m. Program n m => Proxy n -> Proxy m
add _ = Proxy

test = add (Proxy :: Proxy 1)
