module Main where

class MyConstraint :: Constraint
class MyConstraint

constrained :: MyConstraint => Int
constrained = 42

removeConstraint :: forall a. (MyConstraint => a) -> a
removeConstraint x = x

test :: Int
test = removeConstraint constrained

test' = removeConstraint constrained
