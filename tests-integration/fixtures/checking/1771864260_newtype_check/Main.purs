module Main where

newtype Tagged :: forall k. k -> Type -> Type
newtype Tagged t a = Tagged a
