module ValueEquationGuardMultiple where

f a b
  | c <- a
  , d <- b 
  , c << d = e
