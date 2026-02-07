module Main where

class Generate f where
  generate :: forall a b. (b -> a) -> b -> f a

data List a = Nil | Cons a

instance Generate List where
  generate f b = Cons (f b)

test :: Int -> List Int
test start
  | true = generate (\x -> x) start
  | true = generate (\x -> x) start
