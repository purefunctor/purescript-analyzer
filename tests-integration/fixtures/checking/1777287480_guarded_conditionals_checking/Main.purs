module Main where

class Build f where
  singleton :: forall a. a -> f a
  produce :: forall a b. (b -> a) -> b -> f a

identity :: forall a. a -> a
identity x = x

test :: forall f. Build f => Boolean -> Boolean -> f Boolean
test = case _, _ of
  from, to
    | from -> singleton true
    | to -> produce identity true
    | true -> produce identity false
