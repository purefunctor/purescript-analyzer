module Main where

data F f a = F (f a)

type role F representational phantom
