module Main where

import Data.Functor (class Functor)

-- Class members using a type synonym with forall, combined with
-- fundeps and given constraints from the instance context.

type Transform :: (Type -> Type) -> (Type -> Type) -> Type
type Transform f g = forall a. f a -> g a

infixr 4 type Transform as ~>

class (Functor m, Functor f) <= Parallel (f :: Type -> Type) (m :: Type -> Type) | m -> f, f -> m where
  parallel :: m ~> f
  sequential :: f ~> m

newtype ReaderT r (m :: Type -> Type) a = ReaderT (r -> m a)

mapReaderT :: forall r m n a b. (m a -> n b) -> ReaderT r m a -> ReaderT r n b
mapReaderT f (ReaderT g) = ReaderT (\r -> f (g r))

instance (Parallel f m) => Parallel (ReaderT e f) (ReaderT e m) where
  parallel = mapReaderT parallel
  sequential = mapReaderT sequential
