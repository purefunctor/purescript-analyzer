module Main where

type ForConst a = forall b. b -> a

const :: forall a. a -> ForConst a
const a b = a

type TripleForall a = forall b. forall c. b -> c -> a

triple :: forall a. a -> TripleForall a
triple a b c = a

type Flip a = forall b c. (a -> b -> c) -> b -> a -> c

flip :: forall a. Flip a
flip f b a = f a b
