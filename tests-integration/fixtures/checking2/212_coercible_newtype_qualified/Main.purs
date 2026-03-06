module Main where

import Lib as L
import Safe.Coerce (coerce)

coerceQualified :: Int -> L.Age
coerceQualified = coerce

unwrapQualified :: L.Age -> Int
unwrapQualified = coerce
