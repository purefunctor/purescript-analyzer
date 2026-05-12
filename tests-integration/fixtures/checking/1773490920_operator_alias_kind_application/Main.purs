module Main where

data Pair :: forall k. k -> k -> Type
data Pair a b = Pair

infix 4 type Pair as &
