module Main where

class Eq a where
  eq :: a -> a -> Boolean

-- Should infer: forall t0. Eq t0 => t0 -> t0 -> Boolean
test x y = eq x y

-- Multiple constraints
class Ord a where
  compare :: a -> a -> Int

-- Should infer: forall t0. Ord t0 => t0 -> t0 -> Int
test2 x y = compare x y
