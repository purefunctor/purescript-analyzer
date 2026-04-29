module Main where

foreign import add :: Int -> Int -> Int

test = add ?one ?two
--         $    $
