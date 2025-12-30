module Main where

import Prim.Int (class Add, class Mul, class Compare, class ToString)
import Prim.Ordering (Ordering, LT, EQ, GT)

data Proxy :: forall k. k -> Type
data Proxy a = Proxy

data Unit = Unit

-- Add: Derive sum from operands
deriveSum :: forall sum. Add 1 2 sum => Proxy sum
deriveSum = Proxy

-- Add: Derive right operand from left and sum
deriveRight :: forall right. Add 1 right 3 => Proxy right
deriveRight = Proxy

-- Add: Derive left operand from right and sum
deriveLeft :: forall left. Add left 2 3 => Proxy left
deriveLeft = Proxy

-- Mul: Derive product from operands
deriveMul :: forall product. Mul 3 4 product => Proxy product
deriveMul = Proxy

-- Compare: Derive ordering from operands
compareLT :: forall ord. Compare 1 2 ord => Proxy ord
compareLT = Proxy

compareEQ :: forall ord. Compare 5 5 ord => Proxy ord
compareEQ = Proxy

compareGT :: forall ord. Compare 10 3 ord => Proxy ord
compareGT = Proxy

-- ToString: Derive symbol from integer
deriveString :: forall s. ToString 42 s => Proxy s
deriveString = Proxy

-- Force the solver to resolve all constraints
forceSolve = { deriveSum, deriveRight, deriveLeft, deriveMul, compareLT, compareEQ, compareGT, deriveString }
