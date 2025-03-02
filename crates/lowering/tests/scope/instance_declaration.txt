module Main where

instance Eq Int where
  eq :: Int -> Int -> Boolean
  eq = eqIntImpl

instance Eq  => Eq (Maybe a) where
  eq :: Maybe a -> Maybe a -> Boolean
  eq = eqMaybeImpl

instance TypeEquals b b where
  proof :: forall p. p b -> p b
  proof b = b
