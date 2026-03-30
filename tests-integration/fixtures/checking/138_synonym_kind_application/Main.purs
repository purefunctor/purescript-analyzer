module Main where

data RowBox :: Row Type -> Type
data RowBox r

type Apply :: forall k. (k -> Type) -> k -> Type
type Apply f a = f a

type RowDesugared = Apply RowBox ()
