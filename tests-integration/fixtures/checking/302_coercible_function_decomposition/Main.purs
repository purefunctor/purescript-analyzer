module Main where

import Safe.Coerce (class Coercible, coerce)
import Data.Newtype (class Newtype)

newtype Age = Age Int

derive instance Newtype Age _

coerceFn :: (Age -> Int) -> (Int -> Age)
coerceFn = coerce

over :: forall t a s b. Newtype t a => Newtype s b => (a -> t) -> (a -> b) -> t -> s
over _ = coerce

under :: forall t a s b. Newtype t a => Newtype s b => (a -> t) -> (t -> s) -> a -> b
under _ = coerce

alaF
  :: forall f g t a s b
   . Coercible (f t) (f a)
  => Coercible (g s) (g b)
  => Newtype t a
  => Newtype s b
  => (a -> t)
  -> (f t -> g s)
  -> f a
  -> g b
alaF _ = coerce
