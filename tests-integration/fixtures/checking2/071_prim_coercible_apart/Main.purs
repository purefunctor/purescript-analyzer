module Main where

import Safe.Coerce (coerce)

data Maybe a = Nothing | Just a
data Either a b = Left a | Right b

-- Different type constructors: should fail
coerceDifferent :: Maybe Int -> Either Int String
coerceDifferent = coerce
