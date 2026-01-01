module Main where

-- Class with functional dependency: knowing `a` determines `b`
class Convert a b | a -> b where
  convert :: a -> b

instance Convert Int String where
  convert _ = "int"

-- Use convert without specifying the result type explicitly.
-- The constraint `Convert Int ?u` has:
--   position 0 (Int): Match
--   position 1 (?u): Stuck (unification variable)
-- But fundep `a -> b` means position 0 determines position 1,
-- so after closure, position 1 should be adjusted to Match.
-- The fundep allows `?u` to be improved to `String`.
test = convert 42
