module Main where

newtype Fix :: forall k. ((k -> Type) -> k -> Type) -> k -> Type
newtype Fix f a = Fix (f (Fix f) a)
