module Main where

import Data.Functor (class Functor, map)

newtype Wrap :: forall k. Type -> (k -> Type) -> k -> Type
newtype Wrap e w a = Wrap (w a)

instance Functor w => Functor (Wrap e w) where
  map f (Wrap x) = Wrap (map f x)
