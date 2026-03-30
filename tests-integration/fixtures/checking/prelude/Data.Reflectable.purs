module Data.Reflectable where

import Type.Proxy (Proxy)

class Reflectable :: forall k. k -> Type -> Constraint
class Reflectable v t | v -> t where
  reflectType :: Proxy v -> t
