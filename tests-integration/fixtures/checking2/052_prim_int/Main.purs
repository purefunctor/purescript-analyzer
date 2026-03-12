module Main where

import Prim.Int (class Add, class Compare, class Mul, class ToString)
import Prim.Ordering (EQ, GT, LT)
import Type.Proxy (Proxy(..))

deriveSum :: forall sum. Add 1 2 sum => Proxy sum
deriveSum = Proxy

deriveRight :: forall right. Add 1 right 3 => Proxy right
deriveRight = Proxy

deriveLeft :: forall left. Add left 2 3 => Proxy left
deriveLeft = Proxy

stuckAdd :: forall left sum. Add left 1 sum => Proxy left -> Proxy sum
stuckAdd _ = Proxy

deriveMul :: forall product. Mul 3 4 product => Proxy product
deriveMul = Proxy

compareLT :: forall ord. Compare 1 2 ord => Proxy ord
compareLT = Proxy

compareEQ :: forall ord. Compare 5 5 ord => Proxy ord
compareEQ = Proxy

compareGT :: forall ord. Compare 10 3 ord => Proxy ord
compareGT = Proxy

deriveString :: forall s. ToString 42 s => Proxy s
deriveString = Proxy

forceSolve =
  { deriveSum
  , deriveRight
  , deriveLeft
  , deriveMul
  , compareLT
  , compareEQ
  , compareGT
  , deriveString
  , keepStuck: stuckAdd (Proxy :: Proxy 0)
  }
