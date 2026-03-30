module Control.Category where

import Data.Semigroupoid (class Semigroupoid)

class Semigroupoid a <= Category a where
  identity :: forall t. a t t

instance categoryFn :: Category (->) where
  identity x = x
