module Main where

import Lib (Maybe(..), plus, (+), type (:), class Eq)
--          %           %     %         %         %

test1 (Just _) = Just

test2 (_ + _) = [1 + 2, (+)]

type Test3 = Maybe (Maybe Int)

type Test4 = Int : Int : Int

type Test5 = Prim.Int

type Test6 = (:)

test7 :: Eq => Int
test7 = plus 123 123
