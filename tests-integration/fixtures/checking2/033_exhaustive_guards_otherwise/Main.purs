module Main where

import Data.Boolean (otherwise)

data Maybe a = Just a | Nothing

test1 = case _ of
  Just _ | otherwise -> 1

test2 = case _ of
  Nothing | true -> 2
