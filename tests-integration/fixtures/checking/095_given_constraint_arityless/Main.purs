module Main where

class Eq a where
  eq :: a -> a -> Boolean

class Coercible a b where
  coerce :: a -> b

-- Arity 0: constraint used in lambda body
eqSelf :: forall a. Eq a => a -> Boolean
eqSelf = \x -> eq x x

-- Arity 0: multiple constraints
eqCoerce :: forall a b. Eq a => Coercible a b => a -> b
eqCoerce = \x -> if eq x x then coerce x else coerce x

-- Arity 0: nested constraint usage
eqBoth :: forall a b. Eq a => Eq b => a -> b -> Boolean
eqBoth = \x y -> if eq x x then eq y y else eq y y
