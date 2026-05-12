module Main where

import Lib (HiddenAge)
import Lib as L
import Safe.Coerce (coerce)

coerceHidden :: Int -> HiddenAge
coerceHidden = coerce

coerceQualified :: Int -> L.HiddenAge
coerceQualified = coerce
