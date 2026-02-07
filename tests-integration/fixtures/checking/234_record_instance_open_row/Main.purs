module Main where

class Clone :: Type -> Type -> Constraint
class Clone a b | a -> b where
  clone :: a -> b

instance Clone { | r } { | r } where
  clone x = x

clonePerson :: { name :: String, age :: Int } -> { name :: String, age :: Int }
clonePerson = clone

cloneEmpty :: {} -> {}
cloneEmpty = clone

cloneSingle :: { x :: Int } -> { x :: Int }
cloneSingle = clone

class Nest :: Type -> Type -> Constraint
class Nest a b | a -> b where
  nest :: a -> b

instance Nest { | r } { inner :: { | r }, outer :: Int } where
  nest x = { inner: x, outer: 0 }

testNest :: { a :: String } -> { inner :: { a :: String }, outer :: Int }
testNest = nest
