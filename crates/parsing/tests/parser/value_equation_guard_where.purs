module ValueEquationGuardWhere where

f a b
  | c <- a
  , d <- b 
  , c << d = e
  where
  e = c + d
