module Main where

import Safe.Coerce (coerce)

newtype Age = Age Int

coerceArray :: Array Age -> Array Int
coerceArray = coerce

coerceArrayReverse :: Array Int -> Array Age
coerceArrayReverse = coerce
