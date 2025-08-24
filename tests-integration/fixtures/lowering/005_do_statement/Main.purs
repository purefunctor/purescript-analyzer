module Main where

main = do
  action x y z
  let x = 42
  y <- pure 42
  let z = 42
  pure $ x + y + z
