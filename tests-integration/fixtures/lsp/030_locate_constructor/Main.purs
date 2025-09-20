module Main where

data Maybe a = Just a | Nothing
--             %        %

just = Just
nothing = Nothing

newtype Id a = Id a
--             %

id = Id
