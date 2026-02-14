module Main where

partialLet :: Partial => Int
partialLet =
  let
    value = case 123 of
      123 -> 123
  in
    value

partialLet' =
  let
    value = case 123 of
      123 -> 123
  in
    value

partialWhere :: Partial => Int
partialWhere = value
  where
  value = case 123 of
    123 -> 123

partialWhere' = value
  where
  value = case 123 of
    123 -> 123
