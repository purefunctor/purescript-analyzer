module Main where

type ConstPoly = forall a b. a -> b -> a

poly0 :: ConstPoly
poly0 = \a b -> a

poly1 :: ConstPoly
poly1 a b = a

poly2 :: ConstPoly
poly2 a = \b -> a

poly3 :: ConstPoly
poly3 = \a -> \b -> a

poly4 :: forall a b. a -> b -> a
poly4 a b = poly1 a b

poly5 :: forall a b. a -> b -> a
poly5 a = poly2 a
