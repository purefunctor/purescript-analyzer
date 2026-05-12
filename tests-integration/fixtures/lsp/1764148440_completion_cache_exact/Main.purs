module Main where

type Test1 = M
--            ^

type Test2 = M
--            ~

test1 = J
--       ^

test2 = J
--       ~

qual1 = Data.Maybe.
--                 ^

qual2 = Data.Maybe.
--                 ~

type Qual1 = Data.Maybe.
--                      ^

type Qual2 = Data.Maybe.
--                      ~
