module Main where

import Prim.Int.Compare.Proofs

-- Invalid: trying to prove 10 < 1 via transLt (requires 10 < n < 1, impossible)
invalidTransLt :: Proxy ( left :: 10, right :: 1 )
invalidTransLt = transLt (Proxy :: Proxy 5)

-- Invalid: trying to prove 1 > 10 via transGt (requires 1 > n > 10, impossible)
invalidTransGt :: Proxy ( left :: 1, right :: 10 )
invalidTransGt = transGt (Proxy :: Proxy 5)

-- Invalid: direct comparison failures
invalidLt :: Proxy ( left :: 5, right :: 1 )
invalidLt = assertLesser

invalidGt :: Proxy ( left :: 1, right :: 5 )
invalidGt = assertGreater

invalidEq :: Proxy ( left :: 1, right :: 5 )
invalidEq = assertEqual
