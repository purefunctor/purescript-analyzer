module Main where

foreign import data Collector :: Type -> Type -> Type

foreign import data Tuple :: Type -> Type -> Type

foreign import map :: forall a b x y. (a -> b) -> Collector x a -> Collector (Tuple x y) b

foreign import apply :: forall a b x y. Collector x (a -> b) -> Collector y a -> Collector (Tuple x y) b

foreign import pure :: forall a. a -> Collector a a

test1 = ado
  x <- pure 0
  y <- pure ""
  z <- pure 'a'
  in { x, y, z }

test2 = ado
  x <- pure 'a'
  y <- pure true
  in { x, y }

test3 = ado
  x <- pure 0
  z <- pure ""
  in { x, z }
