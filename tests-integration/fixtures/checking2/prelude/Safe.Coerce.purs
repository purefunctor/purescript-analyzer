module Safe.Coerce (coerce, module Prim.Coerce) where

import Prim.Coerce (class Coercible)

coerce :: forall a b. Coercible a b => a -> b
coerce = unsafeCoerce

foreign import unsafeCoerce :: forall a b. a -> b
