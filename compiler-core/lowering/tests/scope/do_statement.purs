module Main where

main = do
  action x y
  let x = 42
  y <- pure 42
  let z = 42
  pure $ x + y + z
