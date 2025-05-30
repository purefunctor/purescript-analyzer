module TypeParenthesizedFunction where

function :: forall a b. ((a -> b) -> (a -> b))
