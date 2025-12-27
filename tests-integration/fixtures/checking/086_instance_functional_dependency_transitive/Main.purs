module Main where

-- Transitive functional dependencies: a -> b, b -> c
-- If we know `a`, we can determine `b`, and knowing `b` determines `c`
class Chain a b c | a -> b, b -> c where
  chain :: a -> c

instance Chain Int String Boolean where
  chain _ = true

-- The constraint `Chain Int ?u ?v` has:
--   position 0 (Int): Match
--   position 1 (?u): Stuck
--   position 2 (?v): Stuck
-- Fundep closure:
--   Initial: {0}
--   After `a -> b`: {0, 1}
--   After `b -> c`: {0, 1, 2}
-- So both Stuck positions should be adjusted to Match
test = chain 42
