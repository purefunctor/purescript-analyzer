module Main where

label = { a: 1, b: "hello" }

field a b = { a, b }

id :: forall a. a -> a
id a = a

polymorphic = { a: id, b: id }
