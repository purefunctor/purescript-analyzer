module Main where

import Data.Eq (class Eq, class Eq1)

data Wrap f a = MkWrap (f a)

derive instance (Eq1 f, Eq a) => Eq (Wrap f a)

-- Should fail: missing Eq1 f constraint
data WrapNoEq1 f a = MkWrapNoEq1 (f a)

derive instance Eq a => Eq (WrapNoEq1 f a)
