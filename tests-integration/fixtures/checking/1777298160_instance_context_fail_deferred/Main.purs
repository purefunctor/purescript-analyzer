module Main where

import Prim.TypeError (class Fail, Text)

class LazyFail a where
  lazyFail :: a

instance Fail (Text "LazyFail") => LazyFail Int where
  lazyFail = 42
