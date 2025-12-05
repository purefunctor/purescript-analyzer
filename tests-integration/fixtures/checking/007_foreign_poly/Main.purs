module Main where

foreign import data TuplePoly :: forall a b. a -> b -> Type

type InferTuplePoly x y = TuplePoly x y
