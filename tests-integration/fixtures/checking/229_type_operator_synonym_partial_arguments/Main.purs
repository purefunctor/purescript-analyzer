module Main where

type RowApply :: forall k. (Row k -> Row k) -> Row k -> Row k
type RowApply f a = f a

infixr 0 type RowApply as +

type AddField :: Type -> Row Type -> Row Type
type AddField a r =
  ( field :: a
  | r
  )

type Bad :: Type -> Row Type
type Bad a = AddField a + ()

type Good :: Type -> Row Type
type Good a = RowApply (AddField a) ()
