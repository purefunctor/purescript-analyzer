module Main where

infixl 6 add as +

data Tuple a b = Tuple a b

add :: Int -> Int -> Int
add x y = x

negate :: Int -> Int
negate x = x

-- Int -> Int
test1 = (_ + 1)

-- Int -> Int
test2 = (1 + _)

-- Int -> Int -> Int
test3 = (_ + _)

-- { foo :: t | r } -> t
test4 = _.foo

-- { foo :: t | r } -> { foo :: Int | r }
test5 = _ { foo = 42 }

-- Boolean -> a -> a -> a
test6 = if _ then _ else _

-- Int -> Int
test7 = case _ of
  1 -> 2
  _ -> 0

-- Int -> Int
test8 = add 1 _

-- Int -> Int -> Int
test9 = add _ _

-- ((Int -> Int) -> a) -> a
test10 f = f (_ + 1)

-- a -> Array a
test11 = [_]

-- a -> { foo :: a }
test12 = { foo: _ }

-- a -> a
test13 = (_)

-- a -> a -> Array a
test14 = [_, _]

-- a -> b -> { a :: a, b :: b }
test15 = { a: _, b: _ }

-- a -> Tuple a Int
test16 = Tuple _ 42

-- Tuple String Int
test17 = (Tuple _ 42) "life"

-- Int -> Int
test18 = - _
