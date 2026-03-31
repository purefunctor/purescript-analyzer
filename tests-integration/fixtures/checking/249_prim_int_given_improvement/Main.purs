module Main where

import Prim.Int (class Add)
import Type.Proxy (Proxy(..))

addChain :: forall n m. Add 1 2 n => Add 1 n m => Proxy m
addChain = Proxy

addContradiction :: forall m. Add 1 2 4 => Add 1 4 m => Proxy m
addContradiction = Proxy

forceSolve =
  { addChain
  , addContradiction
  }
