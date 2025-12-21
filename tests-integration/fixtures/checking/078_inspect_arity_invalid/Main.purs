module Main where

-- TooManyBinders: more binders than arity

type BinaryOp a = a -> a -> a

addWrong :: forall a. BinaryOp a
addWrong x y z = x

type UnaryOp a = a -> a
type ReturnUnary a = a -> UnaryOp a

composeWrong :: forall a. ReturnUnary a
composeWrong x y z = z

type Const = forall a b. a -> b -> a

constWrong :: Const
constWrong x y z = x
