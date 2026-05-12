module Main where

type UnaryKind = Type -> Type

data Box :: UnaryKind
data Box a = Box a
