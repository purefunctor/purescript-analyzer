module Main where

import Data.Eq (class Eq, class Eq1)
import Data.Ord (class Ord, class Ord1)

data T f g = T (f (g Int))

derive instance (Eq1 f) => Eq (T f g)
derive instance (Ord1 f) => Ord (T f g)

data Maybe a = Nothing | Just a

derive instance Eq a => Eq (Maybe a)
derive instance Ord a => Ord (Maybe a)

data U f = U (f (Maybe Int))

derive instance (Eq1 f) => Eq (U f)
derive instance (Ord1 f) => Ord (U f)

data V f g = V (f (g 42))

derive instance (Eq1 f) => Eq (V f g)
derive instance (Ord1 f) => Ord (V f g)
