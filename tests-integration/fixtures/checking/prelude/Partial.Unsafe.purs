module Partial.Unsafe where

import Safe.Coerce (unsafeCoerce)

unsafePartial :: forall a. (Partial => a) -> a
unsafePartial = unsafeCoerce
