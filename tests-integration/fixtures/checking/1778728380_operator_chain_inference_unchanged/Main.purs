module Main where

infixr 6 prepend as ++

prepend :: forall a. Array a -> Array a -> Array a
prepend xs ys = xs ++ ys

-- Inference mode: no expected type, checker must infer from operator chain
chain :: Array Int -> Array Int -> Array Int
chain = prepend

-- Inference mode: nested operator chain
nested :: Array Int -> Array Int -> Array Int
nested xs ys = prepend (prepend xs ys) ys
