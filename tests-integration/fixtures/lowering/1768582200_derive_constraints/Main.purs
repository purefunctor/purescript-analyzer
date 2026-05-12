module Main where

class Bound a b
class Intermediate a b
class Result a b

derive instance (Intermediate n m, Result m o) => Bound n o

