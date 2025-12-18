module Main where

infixl 6 add as +
infixl 6 sub as -

add :: Int -> Int -> Int
add x y = x

sub :: Int -> Int -> Int
sub x y = x

identity :: forall a. a -> a
identity x = x

-- Int -> String -> { a :: Int, b :: String }
test1 =
  let
    function = { a: _, b: _ }
    unify = function 123 "Hello"
  in
    function

-- Inner: \f x -> f x
-- Outer: (\g -> g (\f x -> f x))
test2 = (_ (_ _))

-- Boolean -> Int -> Int
test3 = (if _ then (_ + 1) else (_ - 1))

-- { foo :: t | r } -> t
-- { foo :: Int -> Int }
-- Int
test4 = (_.foo) { foo: _ + 1 } 5

-- { x :: Int, y :: Int }
test5 =
  let r = { x: 0, y: 0 }
  in (_ { x = _, y = _ }) r 1 2

-- f :: Int -> Int
--
-- g :: ((Int -> Int) -> a) -> a
-- g = \fn -> fn f
--
-- Int
test6 =
  let
    f = _ + 1
    g = _ f
  in
    g (\h -> h 5)
