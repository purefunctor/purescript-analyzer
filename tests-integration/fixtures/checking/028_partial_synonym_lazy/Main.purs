module Main where

type RowApply :: forall k. (Row k -> Row k) -> Row k -> Row k
type RowApply f a = f a

type ExampleRow :: Row Type -> Row Type
type ExampleRow r = ( a :: Int | r )

type F = RowApply ExampleRow ()
