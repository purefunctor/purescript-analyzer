module Main where

class Read a where
  read :: String -> a

class Show a where
  show :: a -> String

-- The intermediate type 'a' doesn't appear in the result type String -> String
-- This should produce an AmbiguousConstraint error
test s = show (read s)
