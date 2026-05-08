module Main where

data Proxy :: forall k. k -> Type
data Proxy a = Proxy

data K1 = K1
data Unit = Unit

class C :: Type -> Row Type -> Constraint
class C key row | key -> row where
  c :: Proxy key -> Proxy row -> Unit

instance C K1 (a :: Int, b :: Boolean) where
  c _ _ = Unit
else instance C key (a :: String, b :: Boolean | r) where
  c _ _ = Unit

test :: forall key. Proxy key -> Unit
test key = c key (Proxy :: Proxy (a :: String, b :: Boolean | _))
