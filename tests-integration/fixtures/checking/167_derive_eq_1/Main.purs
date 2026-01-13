module Main where

import Data.Eq (class Eq, class Eq1)

data Id a = Id a

derive instance Eq a => Eq (Id a)
derive instance Eq1 Id

data Pair a = Pair a a

derive instance Eq a => Eq (Pair a)
derive instance Eq1 Pair

data Mixed a = Mixed Int a Boolean

derive instance Eq a => Eq (Mixed a)
derive instance Eq1 Mixed

data Rec a = Rec { value :: a, count :: Int }

derive instance Eq a => Eq (Rec a)
derive instance Eq1 Rec

data Wrap f a = Wrap (f a)

derive instance (Eq1 f, Eq a) => Eq (Wrap f a)
derive instance Eq1 f => Eq1 (Wrap f)

data Compose f g a = Compose (f (g a))

derive instance (Eq1 f, Eq (g a)) => Eq (Compose f g a)

-- Eq1 for Compose fails: needs Eq (g a) but only has Eq a from signature
derive instance Eq1 f => Eq1 (Compose f g)

data Either' a = Left' a | Right' a

derive instance Eq a => Eq (Either' a)
derive instance Eq1 Either'

-- Should fail: missing Eq instance
data NoEq a = NoEq a

derive instance Eq1 NoEq
