module Data.Traversable where

import Data.Functor (class Functor)
import Data.Foldable (class Foldable)
import Control.Applicative (class Applicative)

class (Functor t, Foldable t) <= Traversable t where
  traverse :: forall a b m. Applicative m => (a -> m b) -> t a -> m (t b)
  sequence :: forall a m. Applicative m => t (m a) -> m (t a)
