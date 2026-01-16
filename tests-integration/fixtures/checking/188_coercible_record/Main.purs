module Main where

import Safe.Coerce (coerce)

newtype Age = Age Int

coerceRecord :: { name :: String, age :: Age } -> { name :: String, age :: Int }
coerceRecord = coerce

coerceRecordReverse :: { name :: String, age :: Int } -> { name :: String, age :: Age }
coerceRecordReverse = coerce

newtype UserId = UserId Int

coerceMultiple :: { age :: Age, id :: UserId } -> { age :: Int, id :: Int }
coerceMultiple = coerce
