module Main where

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

cls :: C
--      ^
cls = unit
