module Main where

newtype Mu f = In (f (Mu f))
