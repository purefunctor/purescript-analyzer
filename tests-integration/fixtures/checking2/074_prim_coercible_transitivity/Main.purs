module Main where

import Safe.Coerce (coerce)

newtype Age = Age Int
newtype Years = Years Age

coerceTransitive :: Int -> Years
coerceTransitive = coerce

unwrapTransitive :: Years -> Int
unwrapTransitive = coerce
