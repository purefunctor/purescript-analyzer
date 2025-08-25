module Prim.Int where

import Prim.Ordering (Ordering)

class Add :: Int -> Int -> Int -> Constraint
class Add left right sum | left right -> sum, left sum -> right, right sum -> left

class Compare :: Int -> Int -> Ordering -> Constraint
class Compare left right ordering | left right -> ordering

class Mul :: Int -> Int -> Int -> Constraint
class Mul left right product | left right -> product

class ToString :: Int -> Symbol -> Constraint
class ToString int string | int -> string
