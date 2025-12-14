module Main where

idRec :: forall a. a -> a
idRec x = choose x x

choose :: forall a. a -> a -> a
choose x _ = idRec x
