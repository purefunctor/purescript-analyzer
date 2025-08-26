module Main where

main = do
  action x y z
  let x = 42
  y <- pure 42
  let z = 42
  pure $ x + y + z

action = 42
pure = 42
x = 42
y = 42
z = 42
