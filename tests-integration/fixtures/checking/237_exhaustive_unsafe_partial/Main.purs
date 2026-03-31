module Main where

import Partial.Unsafe (unsafePartial)

safeZero :: Partial => Int -> Int
safeZero = \0 -> 0

unsafeZeroLambda = unsafePartial \0 -> 0
unsafeZeroName = unsafePartial safeZero
