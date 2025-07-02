module Main where

import Data.Maybe as Data.Maybe
import Lib (life, Maybe(..), (+), type (++))
import Lib as Lib

ref :: Int
ref = l
--     ^

just :: forall a. a -> M
--                      ^
just = J
--      ^

nothing :: forall a. M
--                    ^
nothing = N
--         ^

refQ :: Int
refQ = Lib.l
--          ^

justQ :: forall a. a -> Lib.M
--                           ^
justQ = J
--      ^

nothingQ :: forall a. Lib.M
--                         ^
nothingQ = Lib.N
--              ^

moduleNameQ = Li
--              ^

fullModuleNameQ = Data.M
--                      ^
