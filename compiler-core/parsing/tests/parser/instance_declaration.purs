module InstanceDeclaration where

instance Functor Maybe where
  map :: forall a b. (a -> b) -> Maybe a -> Maybe b
  map f (Just a) = Just (f a)
  map f Nothing = Nothing
