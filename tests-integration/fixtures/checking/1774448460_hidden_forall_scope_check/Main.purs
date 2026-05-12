module Main where

data Unit = Unit

class Monad :: (Type -> Type) -> Constraint
class Monad m

data Proxy :: Type -> Type -> Type -> Type -> (Type -> Type) -> Type -> Type
data Proxy x' x a' a m r = Respond a (a' -> Proxy x' x a' a m r) | Pure r

respond :: forall m a a' x x'. Monad m => a -> Proxy x' x a' a m a'
respond a = Respond a Pure

type Producer_ :: Type -> (Type -> Type) -> Type -> Type
type Producer_ b m r = forall (x' :: Type) (x :: Type). Proxy x' x Unit b m r

yield :: forall m a. Monad m => a -> Producer_ a m Unit
yield = respond
