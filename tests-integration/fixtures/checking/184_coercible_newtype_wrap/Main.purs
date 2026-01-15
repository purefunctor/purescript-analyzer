module Main where

import Safe.Coerce (coerce)

newtype Age = Age Int

wrapAge :: Int -> Age
wrapAge = coerce

unwrapAge :: Age -> Int
unwrapAge = coerce

newtype Wrapper a = Wrapper a

wrapValue :: forall a. a -> Wrapper a
wrapValue = coerce

unwrapValue :: forall a. Wrapper a -> a
unwrapValue = coerce
