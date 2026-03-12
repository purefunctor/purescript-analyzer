module Main where

type RowApply :: forall k. (Row k -> Row k) -> Row k -> Row k
type RowApply f a = f a

infixr 0 type RowApply as +

type AddInt :: Row Type -> Row Type
type AddInt r = ( int :: Int | r )

type AddString :: Row Type -> Row Type
type AddString r = ( string :: String | r )

type Test :: Row Type
type Test = AddInt + AddString + ()

test :: Record Test
test = { int: 42, string: "life" }
