module ValueEquationGuardBinder where

f c
  | a /\ b <- c = { a, b, c }
