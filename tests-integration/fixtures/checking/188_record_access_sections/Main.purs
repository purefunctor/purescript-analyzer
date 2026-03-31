module Main where

map :: forall a b. (a -> b) -> Array a -> Array b
map f x = []

apply :: forall a b. (a -> b) -> a -> b
apply f x = f x

f :: ({ foo :: Int } -> Int) -> Int
f g = g { foo: 1 }

test1 = _.prop

test2 = _.a.b.c

test3 = _.poly

test4 = map _.prop

test5 = f _.foo

test6 r g = g _.bar

test7 = \r -> _.nonexistent r

test8 = (_ _.foo)

test9 = map (_ _.prop)

test10 = apply (_.a.b)

test11 = { a: _, b: _.foo }

test12 = [_, _.bar]

test13 = case _ of
  _ -> _.prop

test14 = if _ then _.a else _.b
