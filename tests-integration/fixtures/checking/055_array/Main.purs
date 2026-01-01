module Main where

id :: forall a. a -> a
id a = a

monomorphic = [1, 2]

polymorphic = [id, id]
