module Main where

-- Deep nesting test cases

type Apply1 :: (Type -> Type) -> Type -> Type
type Apply1 f a = f a

type Apply2 :: (Type -> Type -> Type) -> Type -> Type -> Type
type Apply2 f a b = f a b

type Const :: Type -> Type -> Type
type Const a b = a

type Flip :: (Type -> Type -> Type) -> Type -> Type -> Type
type Flip f a b = f b a

-- Single-argument synonym
type Wrapper :: Type -> Type
type Wrapper a = a

-- Two-argument synonym
type Pair :: Type -> Type -> Type
type Pair a b = a

-- Deeply nested applications
type Deep1 = Apply1 Wrapper Int
type Deep2 = Apply2 Pair Int String
type Deep3 = Apply2 (Flip Pair) Int String

-- Triple nesting
type Apply3 :: ((Type -> Type) -> Type -> Type) -> (Type -> Type) -> Type -> Type
type Apply3 f g a = f g a

type Deep4 = Apply3 Apply1 Wrapper Int

-- Compose pattern
type Compose :: (Type -> Type) -> (Type -> Type) -> Type -> Type
type Compose f g a = f (g a)

type Composed = Apply1 (Compose Wrapper Wrapper) Int
