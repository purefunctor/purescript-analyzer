module Main where

type NatTrans f g = forall a. f a -> g a

apply :: forall f g. NatTrans f g -> f Int -> g Int
apply nat fa = nat fa
