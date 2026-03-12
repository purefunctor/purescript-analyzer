module Main where

type RowApply :: forall k. (Row k -> Row k) -> Row k -> Row k
type RowApply f a = f a

type AddInt :: Row Type -> Row Type
type AddInt r = ( int :: Int | r )

type AddString :: Row Type -> Row Type
type AddString r = ( string :: String | r )

type Test :: Row Type
type Test = RowApply AddInt (RowApply AddString ())

test :: Record Test
test = { int: 42, string: "life" }
