module Main where

class Convert a b | a -> b where
  convert :: a -> b

useGiven :: forall x. Convert Int x => x
useGiven = convert 42

class Relate a b c | a b -> c where
  relate :: a -> b -> c

useRelate :: forall y. Relate Int String y => y
useRelate = relate 1 "hello"
