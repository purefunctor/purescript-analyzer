module Main where

type Const a b = a -> b -> a

const0 :: forall a b. Const a b
const0 = \a b -> a

const1 :: forall a b. Const a b
const1 a b = a

const2 :: forall a b. Const a b
const2 a = \b -> a

const3 :: forall a b. Const a b
const3 = \a -> \b -> a

const4 :: forall a b. a -> b -> a
const4 a b = const1 a b

const5 :: forall a b. a -> b -> a
const5 a = const2 a
