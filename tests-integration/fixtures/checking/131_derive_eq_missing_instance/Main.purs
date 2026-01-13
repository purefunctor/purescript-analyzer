module Main where

import Data.Eq (class Eq)

data NoEq = MkNoEq

data Box = MkBox NoEq

derive instance Eq Box
