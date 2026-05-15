module Main where

data Proxy :: forall k. k -> Type
data Proxy a = Proxy

data Unit = Unit

class Resolve :: Type -> Constraint
class Resolve a where
  resolve :: Proxy a -> Unit

instance recursiveResolve :: Resolve a => Resolve a where
  resolve _ = Unit
else instance resolveInt :: Resolve Int where
  resolve _ = Unit
