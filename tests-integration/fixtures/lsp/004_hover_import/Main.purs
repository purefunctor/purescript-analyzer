module Main where

import Lib (life, Maybe(..), (+),  type (++))
--      $    $     $          $          $

ref :: Int
ref = life
--     $

just :: forall a. a -> Maybe a
--                      $    $
just = Just
--      $

nothing :: forall a. Maybe a
--                    $
nothing = Nothing
--         $

opName :: Int -> Int -> Int
opName = (+)
--        $

opChain :: Int
opChain = 9 + 10
--          $

type TypeOpChain = Int ++ Int
--                     $
