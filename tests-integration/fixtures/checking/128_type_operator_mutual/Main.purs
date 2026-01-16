module Main where

-- This tests mutual recursion between a data type and its operator alias.
-- The operator `+` resolves to `Add`, and `Add` uses `+` in its body.
-- They form an Scc::Mutual, so they must share the same pending kind.

data Add a b = MkAdd (a + b)

infixl 5 type Add as +
