module Effect.Aff where

import Control.Applicative (class Applicative)
import Control.Apply (class Apply)
import Control.Bind (class Bind)
import Control.Monad (class Monad)
import Data.Functor (class Functor)

foreign import data Aff :: Type -> Type

foreign import mapAff :: forall a b. (a -> b) -> Aff a -> Aff b
foreign import applyAff :: forall a b. Aff (a -> b) -> Aff a -> Aff b
foreign import pureAff :: forall a. a -> Aff a
foreign import bindAff :: forall a b. Aff a -> (a -> Aff b) -> Aff b

instance Functor Aff where
  map = mapAff

instance Apply Aff where
  apply = applyAff

instance Applicative Aff where
  pure = pureAff

instance Bind Aff where
  bind = bindAff

instance Monad Aff
