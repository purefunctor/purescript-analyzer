module Main where

class Monad m where
  return :: forall a. a -> m a

class Monad m <= MonadState s m where
  get :: m s
  modify :: (s -> s) -> m s
