module Main where

class HeytingAlgebra a where
  disj :: a -> a -> a

instance heytingAlgebraBoolean :: HeytingAlgebra Boolean where
  disj left _ = left

instance heytingAlgebraFunction :: HeytingAlgebra b => HeytingAlgebra (a -> b) where
  disj left right value = disj (left value) (right value)

test :: Function Char Boolean -> Function Char Boolean -> Function Char Boolean
test = disj
