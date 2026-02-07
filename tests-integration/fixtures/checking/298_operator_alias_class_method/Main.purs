module Main where

import Control.Category (class Category, identity)

-- Operator alias for a class method whose return type
-- is an applied type variable (a t t), not a Function.
infixl 4 identity as <<$>>

test :: (Int -> Int) -> Int -> Int
test f x = f <<$>> x

test' f x = f <<$>> x
