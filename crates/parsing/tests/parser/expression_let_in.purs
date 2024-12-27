module ExpressionLetIn where

let_in = let x = y in z

let_in =
  let x = y
  in z

let_in =
  let
    x = y
  in
    z

let_in =
  let
    life :: Int
    life = 42
  in
    life
