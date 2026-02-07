module Main where

import Partial.Unsafe (unsafePartial)

data Maybe a = Just a | Nothing

fromJust :: forall a. Partial => Maybe a -> a
fromJust (Just a) = a

deleteAt :: forall a. Int -> Array a -> Maybe (Array a)
deleteAt _ _ = Nothing

apply :: forall a b. (a -> b) -> a -> b
apply f x = f x

infixr 0 apply as $

-- apply ($) discharging Partial from a simple expression
test :: Int
test = unsafePartial $ fromJust (Just 42)

test' = unsafePartial $ fromJust (Just 42)

-- apply ($) discharging Partial from a more complex expression
test2 :: Array Int -> Array Int
test2 ys = unsafePartial $ fromJust (deleteAt 0 ys)
