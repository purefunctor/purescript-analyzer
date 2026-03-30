module Main where

import Safe.Coerce (coerce)

data Proxy a = Proxy

coerceProxy :: forall a b. Proxy a -> Proxy b
coerceProxy = coerce

coerceProxyIntString :: Proxy Int -> Proxy String
coerceProxyIntString = coerce
