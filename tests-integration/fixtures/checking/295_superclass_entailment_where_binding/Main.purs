module Main where

import Control.Applicative (class Applicative, pure)
import Control.Bind (class Bind, bind)
import Control.Monad (class Monad)
import Control.Monad.Rec (class MonadRec, tailRecM)
import Data.Functor (class Functor, map)

-- Where-binding uses `pure` with the outer MonadRec constraint.
-- The where-binding's type variable is only unified with the
-- outer skolem after its body is checked, so the constraint
-- solver must emit equalities for stuck given positions.
test :: forall m a. MonadRec m => a -> m a
test a = go a
  where
  go x = pure x

-- needs Bind, via MonadRec => Monad => Bind
test2 :: forall m a. MonadRec m => m a -> m a
test2 ma = go ma
  where
  go x = bind x pure

-- needs Functor, via MonadRec => Monad => Apply => Functor
test3 :: forall m. MonadRec m => m Int -> m Int
test3 mi = go mi
  where
  go x = map (\y -> y) x
