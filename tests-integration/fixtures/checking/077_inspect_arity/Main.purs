module Main where

-- Basic partial application: fewer binders than arity
--
-- foo :: forall a. a -> a -> Int
--
-- signature.arguments := [a, a]
-- signature.result    := Int
--
-- 0 binders := forall a. a -> a -> Int
-- 1 binder  := a -> Int
-- 2 binders := Int

foo0 :: forall a. a -> a -> Int
foo0 = \a b -> 42

foo1 :: forall a. a -> a -> Int
foo1 a = \b -> 42

foo2 :: forall a. a -> a -> Int
foo2 a b = 42

-- Type synonym that expands to function in return position
--
-- type ReturnsInt a = a -> Int
--
-- bar :: forall a. ReturnsInt a -> ReturnsInt a
--
-- signature.arguments := [ReturnsInt a, a]
-- signature.result    := Int
--
-- 0 binders := forall a. ReturnsInt a -> ReturnsInt a
-- 1 binder  := a -> Int
-- 2 binders := Int

type ReturnsInt a = a -> Int

bar0 :: forall a. ReturnsInt a -> ReturnsInt a
bar0 = \f -> f

bar1 :: forall a. ReturnsInt a -> ReturnsInt a
bar1 f = f

bar2 :: forall a. ReturnsInt a -> ReturnsInt a
bar2 f x = 42
