module Main where

class Bound a b
class Intermediate a b
class Result a b

instance (Intermediate n m, Result m o) => Bound n o
