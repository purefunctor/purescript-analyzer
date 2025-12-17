module Main where

foreign import data Collector :: Type -> Type -> Type

foreign import data Tuple :: Type -> Type -> Type

foreign import bind :: forall a b x y. Collector x a -> (a -> Collector y b) -> Collector (Tuple x y) b

foreign import discard :: forall a b x y. Collector x a -> (a -> Collector y b) -> Collector (Tuple x y) b

foreign import pure :: forall a. a -> Collector a a

test1 = do
  x <- pure 0
  y <- pure ""
  z <- pure 'a'
  pure { x, y, z }

test2 = do
  x <- pure 'a'
  y <- pure true
  pure { x, y }

test3 = do
  x <- pure 0
  let y = x
  z <- pure ""
  pure { y, z }
