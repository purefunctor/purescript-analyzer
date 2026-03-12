module Main where

import Lib
import Safe.Coerce (coerce)

coerceOpen :: Int -> HiddenAge
coerceOpen = coerce
