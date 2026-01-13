module Data.Newtype where

import Prim.Coerce (class Coercible)
import Safe.Coerce (coerce)

class Coercible t a <= Newtype t a | t -> a

wrap :: forall t a. Newtype t a => a -> t
wrap = coerce

unwrap :: forall t a. Newtype t a => t -> a
unwrap = coerce
