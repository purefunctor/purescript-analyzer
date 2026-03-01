module Main where

import Lib (HiddenAge)
import Safe.Coerce (coerce)

coerceHidden :: Int -> HiddenAge
coerceHidden = coerce
