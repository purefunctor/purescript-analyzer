module Main where

class Make :: Type -> Type -> Constraint
class Make a b | a -> b where
  make :: a -> b

instance Make { | r } { | r } where
  make x = x

testMake :: { a :: Int, b :: String } -> { a :: Int, b :: String }
testMake = make

class Convert :: Type -> Type -> Constraint
class Convert a b | a -> b where
  convert :: a -> b

instance Convert { | r } { converted :: { | r } } where
  convert x = { converted: x }

testConvert :: { x :: Int } -> { converted :: { x :: Int } }
testConvert = convert
