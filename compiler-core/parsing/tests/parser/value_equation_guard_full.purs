module ValueEquationGuardFull where

f a b
  | c <- a
  , d <- b
  , c << d = e
  where
  e = c + d

  | f <- g
  , h <- i
  , j << k = l
  where
  l = j + k
