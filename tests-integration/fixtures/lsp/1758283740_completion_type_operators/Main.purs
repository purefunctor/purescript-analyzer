module Main where

type Add a b = 5

infix 5 type Add as ++

type Test1 = 
--           ^

type Test2 = +
--            ^

type Test3 = ()
--            ^

type Test4 = (+)
--            ^

type Test5 = (+)
--             ^
