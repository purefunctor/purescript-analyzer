module Main where

class HasKind' a where
  reflectKind' :: forall p. p a -> String
