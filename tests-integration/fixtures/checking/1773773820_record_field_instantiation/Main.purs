module Main where

data Maybe a = Just a | Nothing

constructor state = state { example = Nothing }

variable state =
  let nothing = Nothing
  in state { example = nothing }
