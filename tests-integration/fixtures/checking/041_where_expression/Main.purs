module Main where

identity = impl
  where
  impl a = a

compose = impl
  where
  impl f g x = f (g x)

apply = impl
  where
  impl :: forall a. a -> a
  impl x = x

nested = outer
  where
  outer x = inner
    where
    inner = x

multiPattern = impl
  where
  impl true = 1
  impl false = 0

factorial n = impl n
  where
  impl 0 = 1
  impl m = m
