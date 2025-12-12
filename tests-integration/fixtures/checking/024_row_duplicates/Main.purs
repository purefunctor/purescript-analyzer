module Main where

-- Row with duplicate labels (valid in PureScript rows)
type DuplicateRow = ( a :: Int, a :: String, b :: Boolean )
