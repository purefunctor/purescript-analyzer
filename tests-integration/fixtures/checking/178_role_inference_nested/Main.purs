module Main where

data Maybe a = Nothing | Just a

newtype First a = First (Maybe a)
