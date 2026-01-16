module Data.Bitraversable where

import Data.Bifunctor (class Bifunctor)
import Data.Bifoldable (class Bifoldable)
import Data.Traversable (class Traversable)
import Control.Applicative (class Applicative)

class (Bifunctor t, Bifoldable t) <= Bitraversable t where
  bitraverse :: forall f a b c d. Applicative f => (a -> f c) -> (b -> f d) -> t a b -> f (t c d)
  bisequence :: forall f a b. Applicative f => t (f a) (f b) -> f (t a b)
