module Main where

import Lib
--     %

test1 (Just _) = Just
--     %         %

test2 (_ + _) = [1 + 2, (+)]
--       %         %     %

type Test3 = Maybe (Maybe Int)
--           %

type Test4 = Int : Int : Int
--               %

type Test5 = Int
--           %
