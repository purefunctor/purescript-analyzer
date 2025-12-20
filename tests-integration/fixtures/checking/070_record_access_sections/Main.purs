module Main where

map :: forall a b. (a -> b) -> Array a -> Array b
map f x = []

apply :: forall a b. (a -> b) -> a -> b
apply f x = f x

f :: ({ foo :: Int } -> Int) -> Int
f g = g { foo: 1 }

-- { prop :: t | r } -> t
test1 = _.prop

-- { a :: { b :: { c :: t | rc } | rb } | ra } -> t
test2 = _.a.b.c

-- { poly :: t | r } -> t
test3 = _.poly

-- Array { prop :: t | r } -> Array t
test4 = map _.prop

-- See signature of `f`
test5 = f _.foo

-- a -> (({ bar :: t | r } -> t) -> b) -> b
test6 r g = g _.bar

-- { nonexistent :: t | r } -> t
test7 = \r -> _.nonexistent r

-- (({ foo :: t | r } -> t) -> a) -> a
test8 = (_ _.foo)

-- Array (({ prop :: t | r } -> t) -> a) -> Array a
test9 = map (_ _.prop)

-- { a :: { b :: t | rb } | ra } -> t
test10 = apply (_.a.b)

-- a -> { a :: a, b :: { foo :: t | r } -> t }
test11 = { a: _, b: _.foo }

-- ({ bar :: t | r } -> t) -> Array ({ bar :: t | r } -> t)
test12 = [_, _.bar]

-- a -> { prop :: t | r } -> t
test13 = case _ of
  _ -> _.prop

-- Boolean -> { a :: t | ( b :: t | br ) } -> t
test14 = if _ then _.a else _.b
