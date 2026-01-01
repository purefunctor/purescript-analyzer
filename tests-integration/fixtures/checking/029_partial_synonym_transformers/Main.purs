module Main where

data Identity a = Identity a

newtype ReaderT :: Type -> (Type -> Type) -> Type -> Type
newtype ReaderT r m a = ReaderT (r -> m a)

newtype StateT :: Type -> (Type -> Type) -> Type -> Type
newtype StateT s m a = StateT (s -> m (Tuple a s))

data Tuple a b = Tuple a b

-- Type synonyms specializing to Identity base
type Reader r = ReaderT r Identity
type State s = StateT s Identity

-- Apply pattern for transformers
type Apply :: forall k l. (k -> l) -> k -> l
type Apply f a = f a

-- Using Apply with transformer synonyms
type ReaderInt = Apply (Reader Int) String
type StateString = Apply (State String) Int

-- Nested application
type NestedReader = Apply (Apply (ReaderT Int) Identity) Boolean
