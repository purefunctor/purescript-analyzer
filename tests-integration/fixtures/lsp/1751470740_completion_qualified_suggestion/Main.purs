module Main where

refQ :: Int
refQ = Lib.l
--          ^

justQ :: forall a. a -> Lib.M
--                           ^
justQ = Lib.J
--           ^

nothingQ :: forall a. Lib.M
--                         ^
nothingQ = Lib.J
--              ^

clsQ :: Lib.C
--           ^
clsQ = unit

fullQ :: Int
fullQ = Data.Maybe.j
--                  ^

libEmpty :: Int
libEmpty = Lib.
--             ^

fullEmpty :: Int
fullEmpty = Data.Maybe.
--                     ^
