module Control.Monad.Rec where

import Control.Monad (class Monad)

class Monad m <= MonadRec m where
  tailRecM :: forall a b. (a -> m b) -> a -> m b
