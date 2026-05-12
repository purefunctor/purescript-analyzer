module Main where

class Show a where
  show :: a -> String

data A
data B
data C

class TypeEquals :: forall k. k -> k -> Constraint
class TypeEquals a b | a -> b, b -> a

instance TypeEquals a a

class Match3 f g h

instance
  ( TypeEquals f (A -> String)
  , TypeEquals g (B -> String)
  , TypeEquals h (C -> String)
  ) =>
  Match3 f g h

use
  :: forall f g h
   . Match3 f g h
  => f
  -> g
  -> h
  -> Int
use _ _ _ = 0

test :: Show A => Show B => Show C => Int
test = use (\x -> show x) (\y -> show y) (\z -> show z)
