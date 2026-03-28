module Control.Bind where

import Control.Apply (class Apply)
import Data.Unit (Unit)

class Apply m <= Bind m where
  bind :: forall a b. m a -> (a -> m b) -> m b

class Discard a where
  discard :: forall f b. Bind f => f a -> (a -> f b) -> f b

instance discardUnit :: Discard Unit where
  discard = bind
