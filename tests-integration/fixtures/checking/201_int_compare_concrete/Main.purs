module Main where

import Prim.Int.Compare.Proofs

-- Literal comparisons
litLt :: Proxy ( left :: 0, right :: 1 )
litLt = assertLesser

litGt :: Proxy ( left :: 1, right :: 0 )
litGt = assertGreater

litEq :: Proxy ( left :: 0, right :: 0 )
litEq = assertEqual

-- Concrete transitivity proofs
testTransLt :: Proxy ( left :: 1, right :: 10 )
testTransLt = transLt (Proxy :: Proxy 5)

testTransLtEq :: Proxy ( left :: 1, right :: 5 )
testTransLtEq = transLtEq (Proxy :: Proxy 5)

testTransEqLt :: Proxy ( left :: 5, right :: 10 )
testTransEqLt = transEqLt (Proxy :: Proxy 5)

testTransGt :: Proxy ( left :: 10, right :: 1 )
testTransGt = transGt (Proxy :: Proxy 5)

testTransGtEq :: Proxy ( left :: 10, right :: 5 )
testTransGtEq = transGtEq (Proxy :: Proxy 5)

testTransEqGt :: Proxy ( left :: 5, right :: 1 )
testTransEqGt = transEqGt (Proxy :: Proxy 5)

testTransEq :: Proxy ( left :: 5, right :: 5 )
testTransEq = transEq (Proxy :: Proxy 5)

-- Concrete transitivity with symmetry
testTransSymmLt :: Proxy ( left :: 1, right :: 10 )
testTransSymmLt = transSymmLt (Proxy :: Proxy 5)

testTransSymmGt :: Proxy ( left :: 10, right :: 1 )
testTransSymmGt = transSymmGt (Proxy :: Proxy 5)
