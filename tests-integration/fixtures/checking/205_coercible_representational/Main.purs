module Main where

import Safe.Coerce (coerce)

data Maybe a = Nothing | Just a

newtype Age = Age Int

coerceMaybe :: Maybe Age -> Maybe Int
coerceMaybe = coerce

coerceMaybeReverse :: Maybe Int -> Maybe Age
coerceMaybeReverse = coerce

newtype UserId = UserId Int

coerceNested :: Maybe UserId -> Maybe Int
coerceNested = coerce
