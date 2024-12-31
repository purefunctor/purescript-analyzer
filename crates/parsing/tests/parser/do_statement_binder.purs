module DoStatementTupleBinder where

main = do
  a /\ b :: Tuple a b <- Tuple a b
  let 
    a /\ b = Tuple a b
  pure { a, b }
