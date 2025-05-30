module NewtypeVariables where

newtype Mu f = In (f (Mu f))
