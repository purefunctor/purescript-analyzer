module Main where

import Prim.Int (class Add, class Compare)
import Prim.Ordering (GT, LT)

data Proxy :: forall k. k -> Type
data Proxy a = Proxy

data N = N

type MinN :: Int
type MinN = 1

type MaxN :: Int
type MaxN = 5

mkN
  :: forall i lower upper
   . Add MinN (-1) lower
  => Compare i lower GT
  => Add MaxN 1 upper
  => Compare i upper LT
  => Proxy i
  -> N
mkN _ = N

data Something = Something N

mkSomething
  :: forall i lower upper
   . Add MinN (-1) lower
  => Compare i lower GT
  => Add MaxN 1 upper
  => Compare i upper LT
  => Proxy i
  -> Something
mkSomething p = Something (mkN p)

forceSolve :: Something
forceSolve = mkSomething (Proxy :: Proxy 3)
