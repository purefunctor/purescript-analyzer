module Main where

data Identity a = Identity a

type ReaderT :: Type -> (Type -> Type) -> Type -> Type
type ReaderT r m a = r -> m a

type Apply :: forall k1 k2. (k1 -> k2) -> k1 -> k2
type Apply f a = f a

type Good = Apply (Apply (ReaderT Int) Identity) String

type Bad = ReaderT Int
