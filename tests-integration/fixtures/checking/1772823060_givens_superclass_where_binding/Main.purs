module Main where

import Control.Applicative (class Applicative, pure)
import Control.Bind (class Bind, bind)
import Control.Monad (class Monad)
import Control.Monad.Rec (class MonadRec, tailRecM)
import Data.Functor (class Functor, map)

test :: forall m a. MonadRec m => a -> m a
test a = go a
  where
  go x = pure x

test2 :: forall m a. MonadRec m => m a -> m a
test2 ma = go ma
  where
  go x = bind x pure

test3 :: forall m. MonadRec m => m Int -> m Int
test3 mi = go mi
  where
  go x = map (\y -> y) x
