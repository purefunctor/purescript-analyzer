module Main where

import Data.Newtype (class Newtype, unwrap)

newtype Endo :: forall k. (k -> k -> Type) -> k -> Type
newtype Endo c a = Endo (c a a)

instance Newtype (Endo c a) (c a a)

test :: forall b. Endo Function b -> b -> b
test x = unwrap x
