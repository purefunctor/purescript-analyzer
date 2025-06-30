module Main where

life :: Int
life = 42

ref :: Int
ref = life
--    @

arguments :: Int -> Int -> Int -> Array Int
arguments one two three = [one, two, three]
--                          @    @     @

letBindings :: Array Int
letBindings =
  let
    life :: Int
    life = 42
  in
    [life, life]
--    @     @

data Maybe a = Just a | Nothing

just :: Maybe Int
--       @
just = Just 42
--      @

nothing :: Maybe Int
--          @
nothing = Nothing
--         @
