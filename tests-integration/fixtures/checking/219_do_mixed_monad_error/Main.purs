module Main where

import Control.Applicative (pure)
import Control.Bind (bind, discard)
import Effect (Effect)
import Effect.Aff (Aff)

foreign import effect :: Effect Int
foreign import aff :: Aff String

-- Test: error should be attributed to the `aff` line (first conflicting statement)
-- not the `effect` line
test :: Effect { a :: Int, b :: String }
test = do
  a <- effect
  b <- aff
  pure { a, b }

-- Inference variant
test' = do
  a <- effect
  b <- aff
  pure { a, b }
