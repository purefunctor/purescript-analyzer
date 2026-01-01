module Main where

apply = impl
  where
  impl :: forall a. a -> a
  impl x = x

apply' :: forall b. b -> b
apply' = impl
  where
  impl :: forall a. a -> a
  impl x = x
