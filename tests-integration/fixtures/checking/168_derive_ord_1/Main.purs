module Main where

import Data.Eq (class Eq, class Eq1)
import Data.Ord (class Ord, class Ord1)

data Id a = Id a

derive instance Eq a => Eq (Id a)
derive instance Eq1 Id
derive instance Ord a => Ord (Id a)
derive instance Ord1 Id

data Wrap f a = Wrap (f a)

derive instance (Eq1 f, Eq a) => Eq (Wrap f a)
derive instance Eq1 f => Eq1 (Wrap f)
derive instance (Ord1 f, Ord a) => Ord (Wrap f a)
derive instance Ord1 f => Ord1 (Wrap f)

data Compose f g a = Compose (f (g a))

derive instance (Eq1 f, Eq (g a)) => Eq (Compose f g a)
derive instance (Ord1 f, Ord (g a)) => Ord (Compose f g a)

-- Eq1/Ord1 for Compose fails: needs Eq/Ord (g a) but only has Eq/Ord a
derive instance Eq1 f => Eq1 (Compose f g)
derive instance Ord1 f => Ord1 (Compose f g)

-- Should fail: missing Ord instance
data NoOrd a = NoOrd a

derive instance Eq a => Eq (NoOrd a)
derive instance Eq1 NoOrd
derive instance Ord1 NoOrd
