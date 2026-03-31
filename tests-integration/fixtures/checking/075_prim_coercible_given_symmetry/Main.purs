module Main where

import Safe.Coerce (class Coercible, coerce)

test :: forall a b. Coercible b a => a -> b
test = coerce

test' :: forall a b. Coercible b a => a -> b
test' value = coerce value
