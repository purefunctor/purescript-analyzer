module Main where

type Apply :: forall k. (k -> Type) -> k -> Type
type Apply f = f

infixr 5 type Apply as $

type Test1 = Apply Array Int
type Test2 = Array $ Int
