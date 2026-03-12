module Main where

import Safe.Coerce (coerce)

data Maybe a = Nothing | Just a
data Either a b = Left a | Right b

coerceDifferent :: Maybe Int -> Either Int String
coerceDifferent = coerce
