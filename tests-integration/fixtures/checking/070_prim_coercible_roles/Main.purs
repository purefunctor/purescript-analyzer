module Main where

import Safe.Coerce (coerce)

newtype Age = Age Int

-- Phantom: different inner types are fine
data Proxy a = Proxy

coerceProxy :: Proxy Int -> Proxy String
coerceProxy = coerce

-- Representational: Coercible inner propagates
data Maybe a = Nothing | Just a

coerceMaybe :: Maybe Age -> Maybe Int
coerceMaybe = coerce

coerceMaybeReverse :: Maybe Int -> Maybe Age
coerceMaybeReverse = coerce

-- Record coercion (row decomposition)
coerceRecord :: { name :: String, age :: Age } -> { name :: String, age :: Int }
coerceRecord = coerce

-- Function decomposition
coerceFunction :: (Int -> Age) -> (Int -> Int)
coerceFunction = coerce
