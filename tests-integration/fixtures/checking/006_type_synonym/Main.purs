module Main where

foreign import data Tuple :: Type -> Type -> Type

type AliasType = Int

type AliasTypeType = Array

type InferApply f a = f a

type InferTuple x y = Tuple x y

type CheckApply :: forall (x :: Type) (y :: Type). (x -> y) -> x -> y
type CheckApply f a = f a

type CheckApplyElab :: forall x y. (x -> y) -> x -> y
type CheckApplyElab f a = f a
