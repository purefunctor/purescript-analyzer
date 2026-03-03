module Main where

import Data.Functor (class Functor)

data Wrap f a = Wrap (f a)
derive instance Functor f => Functor (Wrap f)

data WrapNoFunctor f a = WrapNoFunctor (f a)
derive instance Functor (WrapNoFunctor f)
