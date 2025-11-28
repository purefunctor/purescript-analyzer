module Main where

data Maybe a = Just a | Nothing

test = 
  let
    c = 3
  in
    [ case _ of
        Just a -> a
        Nothing -> 1
    , case _ of
        Just b -> b
        Nothing -> 2
    ]
