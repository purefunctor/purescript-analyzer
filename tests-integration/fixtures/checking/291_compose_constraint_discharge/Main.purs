module Main where

import Partial.Unsafe (unsafePartial)

data Maybe a = Just a | Nothing

fromJust :: forall a. Partial => Maybe a -> a
fromJust (Just a) = a

compose :: forall a b c. (b -> c) -> (a -> b) -> a -> c
compose f g x = f (g x)

infixr 9 compose as <<<

toArray :: forall a. Maybe a -> Array a
toArray _ = []

-- compose (<<<) discharging Partial through operator chain
test :: forall a b. (Array a -> Maybe b) -> Maybe a -> b
test f = unsafePartial (fromJust <<< f <<< toArray)

test' f = unsafePartial (fromJust <<< f <<< toArray)
