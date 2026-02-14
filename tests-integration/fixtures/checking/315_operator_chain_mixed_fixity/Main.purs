module Main where

import Data.Eq (class Eq)

eq :: forall a. Eq a => a -> a -> Boolean
eq _ _ = true

conj :: Boolean -> Boolean -> Boolean
conj _ _ = true

infix 4 eq as ==
infixr 3 conj as &&

-- Single operator: should work
test1 :: Int -> Boolean
test1 h = h == 2

-- Single &&: should work
test2 :: Boolean
test2 = true && true

-- Mixed chain: the failing case
test3 :: Int -> Int -> Boolean
test3 h rh = h == 2 && rh == 1
