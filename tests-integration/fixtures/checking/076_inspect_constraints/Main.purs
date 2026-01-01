module Main where

class Show a where
  show :: a -> String

class Eq a where
  eq :: a -> a -> Boolean

showIt :: forall a. Show a => a -> String
showIt n = show n

compare :: forall a. Show a => Eq a => a -> a -> String
compare a b = if eq a b then "equal" else show a

type ShowFunction a = Show a => a -> String

showFn :: forall a. ShowFunction a
showFn a = show a

type ReturnsString a = a -> String

constrainedReturn :: forall a. Show a => a -> ReturnsString a
constrainedReturn a b = show b

type NestedConstraint a = forall b. Show b => a -> b -> String

nested :: forall a. Eq a => NestedConstraint a
nested a b = show b
