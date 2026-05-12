module Main where

infixr 0 apply as $

apply :: forall a b. (a -> b) -> a -> b
apply f x = f x

identity :: forall a. a -> a
identity a = a

newtype Wrap a = Wrap a

class Profunctor p where
  dimap :: forall a b c d. (a -> b) -> (c -> d) -> p b c -> p a d

instance profunctorFn :: Profunctor (->) where
  dimap ab cd bc = \a -> cd (bc (ab a))

type Optic p s a = p a a -> p s s

type Lens s a = forall p. Profunctor p => Optic p s a

lens :: forall s a. (a -> s) -> (s -> a) -> Lens s a
lens pack unpack pab = dimap unpack pack pab

wrapped :: forall a. Lens (Wrap a) a
wrapped = lens Wrap $ case _ of Wrap a -> a
