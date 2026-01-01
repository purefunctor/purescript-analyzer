module Main where

-- Multiple determiners: a b -> c
class TypeEq a b c | a b -> c where
  typeEq :: a -> b -> c

instance TypeEq Int Int Boolean where
  typeEq _ _ = true

instance TypeEq String String Boolean where
  typeEq _ _ = false

-- The constraint `TypeEq Int Int ?v` has:
--   position 0 (Int): Match
--   position 1 (Int): Match
--   position 2 (?v): Stuck
-- Fundep `a b -> c` fires since positions 0 and 1 match:
--   Determined: {0, 1, 2}
-- So the Stuck position should be adjusted to Match
test = typeEq 1 2
