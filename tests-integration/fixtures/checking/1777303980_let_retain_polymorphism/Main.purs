module Main where

data Tuple a b = Tuple a b

class Semigroup a where
  append :: a -> a -> a

test :: forall a b. Semigroup a => Semigroup b => a -> a -> b -> b -> Tuple a b
test a1 a2 b1 b2 = Tuple (appendLocal a1 a2) (appendLocal b1 b2)
  where
  appendLocal = append
