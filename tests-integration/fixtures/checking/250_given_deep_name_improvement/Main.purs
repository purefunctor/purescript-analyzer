module Main where

import Prim.Int (class Add)
import Prim.Row as Row
import Type.Proxy (Proxy(..))

needsAdd :: forall x. Add 1 x 3 => Proxy x
needsAdd = Proxy

nonHeadGivenImprovement
  :: forall n
   . Row.Union (a :: Proxy n) () (a :: Proxy 2)
  => Proxy n
nonHeadGivenImprovement = needsAdd

forceSolve =
  { nonHeadGivenImprovement }
