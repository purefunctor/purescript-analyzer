module Main where

add :: Int -> Int -> Int
add x y = x

sub :: Int -> Int -> Int
sub x y = x

mul :: Int -> Int -> Int
mul x y = x

test1 :: Int
test1 = 1 `add` 2

test1' = 1 `add` 2

test2 :: Int
test2 = 1 `add` 2 `sub` 3

test2' = 1 `add` 2 `sub` 3

test3 :: Int
test3 = 1 `add` 2 `sub` 3 `mul` 4

test3' = 1 `add` 2 `sub` 3 `mul` 4

const :: forall a b. a -> b -> a
const x y = x

test4 :: Int
test4 = 1 `const` "hello"

test4' = 1 `const` "hello"

test5 :: Int
test5 = 1 `const` "hello" `add` 2

test5' = 1 `const` "hello" `add` 2

apply :: forall a b. (a -> b) -> a -> b
apply f x = f x

chain :: forall a b c. (b -> c) -> (a -> b) -> a -> c
chain g f x = g (f x)

test6 :: Int
test6 = (\x -> x) `chain` (\x -> x) `apply` 1

test6' = (\x -> x) `chain` (\x -> x) `apply` 1

curry :: forall a b c. (a -> b -> c) -> a -> b -> c
curry f x y = f x y

test7 :: Int -> Int
test7 = add `curry` 1

test7' = add `curry` 1
