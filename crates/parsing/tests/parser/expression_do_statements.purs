module ExpressionDoStatements where

main = do
  let life = 21
  universe <- pure life
  pure $ life + universe
