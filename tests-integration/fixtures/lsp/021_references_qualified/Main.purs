module Main where

import Prim as Prim
import Lib as Lib
--     %

test1 (Lib.Just _) = Lib.Just
--     %             %

test2 (_ Lib.+ _) = [1 Lib.+ 2, Lib.(+)]
--       %             %        %

type Test3 = Lib.Maybe (Lib.Maybe Int)
--           %

type Test4 = Int Lib.: Int Lib.: Int
--               %

type Test5 = Prim.Int
--           %

type Test6 = Lib.(:)
--           %
