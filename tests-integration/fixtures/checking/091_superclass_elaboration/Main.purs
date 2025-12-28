module Main where

-- Define Eq class
class Eq a where
  eq :: a -> a -> Boolean

-- Define Ord with Eq as superclass
class Eq a <= Ord a where
  compare :: a -> a -> Ordering

data Ordering = LT | EQ | GT

-- Test: Ord a => should imply Eq a is available
-- This function has Ord a constraint but uses eq from Eq
test :: forall a. Ord a => a -> a -> Boolean
test x y = eq x y

-- Test with multiple superclass usages
test2 :: forall a. Ord a => a -> a -> Ordering
test2 x y = if eq x y then EQ else compare x y
