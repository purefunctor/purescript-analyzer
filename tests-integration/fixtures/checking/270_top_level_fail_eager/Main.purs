module Main where

import Prim.TypeError (class Fail, Text)

boom :: Fail (Text "top-level Fail was checked") => Int
boom = 0

test :: Int
test = 1
