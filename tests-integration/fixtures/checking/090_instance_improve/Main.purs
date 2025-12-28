module Main where

data True
data False

class TypeEq a b r | a b -> r where
  identity :: a -> b

instance TypeEq a a True where
  identity x = x

-- Checking: identity 42
-- Constraint: TypeEq Int ?b ?r
--   Position 0: Int vs Implicit(a) -> binds a := Int, Match
--   Position 1: ?b vs Bound(a)    -> a bound to Int, Improve (?b, Int)
--   Position 2: ?r vs True        -> Stuck, but fundep {0,1}->{2} determines it
-- After improvements: ?b := Int, ?r := True
-- Expected: test :: Int
test = identity 42
