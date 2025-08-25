module Prim.Coerce where

class Coercible :: forall (k :: Type). k -> k -> Constraint
class Coercible a b
