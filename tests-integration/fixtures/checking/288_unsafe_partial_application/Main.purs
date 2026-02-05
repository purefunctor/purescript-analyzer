module Main where

import Partial.Unsafe (unsafePartial)

data Maybe a = Just a | Nothing

fromJust :: forall a. Partial => Maybe a -> a
fromJust (Just a) = a

-- unsafePartial discharging Partial from an applied expression
test :: Int
test = unsafePartial (fromJust (Just 42))

test' = unsafePartial (fromJust (Just 42))

-- unsafePartial with partial application in map position
mapPartial :: (Int -> Boolean) -> Array Int -> Maybe Int
mapPartial = unsafePartial mapPartialImpl
  where
  mapPartialImpl :: Partial => (Int -> Boolean) -> Array Int -> Maybe Int
  mapPartialImpl _ _ = Just 0
