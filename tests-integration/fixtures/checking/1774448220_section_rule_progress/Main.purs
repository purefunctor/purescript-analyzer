module Main where

import Control.Applicative (class Applicative, pure)
import Control.Apply (class Apply, apply)
import Control.Bind (class Bind, bind)
import Data.Functor (class Functor, map)

data Step a b = Done b | Loop a

class Semigroup a where
  append :: a -> a -> a

class Semigroup a <= Monoid a where
  mempty :: a

class (Applicative m, Bind m) <= MonadRec m where
  tailRecM :: forall a b. (a -> m (Step a b)) -> a -> m b

ifM :: forall m a. Bind m => m Boolean -> m a -> m a -> m a
ifM mb t f = bind mb \b -> if b then t else f

lift2 :: forall f a b c. Apply f => (a -> b -> c) -> f a -> f b -> f c
lift2 f fa fb = apply (map f fa) fb

liftIfM :: forall m f a. Apply f => Bind m => m Boolean -> f (m a) -> f (m a) -> f (m a)
liftIfM p x y = lift2 (ifM p) x y

appendM :: forall m f a b. MonadRec m => Applicative f => Semigroup (f a) => f a -> m a -> m (Step (f a) b)
appendM xs f = bind f \x -> loop (append xs (pure x))

infixr 5 appendM as <+>

loop :: forall m a b. Applicative m => a -> m (Step a b)
loop a = pure (Loop a)

done :: forall m a b. Applicative m => b -> m (Step a b)
done b = pure (Done b)

whileM' :: forall a f m. MonadRec m => Applicative f => Monoid (f a) => m Boolean -> m a -> m (f a)
whileM' p f = tailRecM (liftIfM p (_ <+> f) done) mempty
