module Data.Semigroupoid where

class Semigroupoid a where
  compose :: forall b c d. a c d -> a b c -> a b d

infixr 9 compose as <<<

instance semigroupoidFn :: Semigroupoid (->) where
  compose f g x = f (g x)
