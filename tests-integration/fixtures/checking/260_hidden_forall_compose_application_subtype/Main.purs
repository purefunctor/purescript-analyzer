module Main where

class Semigroupoid :: forall k. (k -> k -> Type) -> Constraint
class Semigroupoid f where
  compose :: forall a b c. f b c -> f a b -> f a c

instance semigroupoidFn :: Semigroupoid (->) where
  compose f g x = f (g x)

newtype Tagged :: Int -> Type -> Type
newtype Tagged n a = Tagged a

type ConcreteTagged :: Type -> Type
type ConcreteTagged a = Tagged 42 a

type HiddenTagged :: Type -> Type
type HiddenTagged a = forall (n :: Int). Tagged n a

make :: forall a. a -> HiddenTagged a
make n = Tagged n

newtype ConcreteNewtype a = ConcreteNewtype (ConcreteTagged a)

test :: forall a. a -> ConcreteNewtype a
test = compose ConcreteNewtype make
