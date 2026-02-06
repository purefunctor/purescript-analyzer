module Main where

data Either a b = Left a | Right b

infixr 6 type Either as \/

in1 :: forall a z. a -> a \/ z
in1 = Left

in2 :: forall a b z. b -> a \/ b \/ z
in2 v = Right (Left v)
