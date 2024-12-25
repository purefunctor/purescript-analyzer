module Binder where

minus -1 = 0
minus -1.0 = 0

constructor (Cons x xs) = 0
constructor Nil = 0

constructor (List.Cons x xs) = 0
constructor List.Nil = 0

identity x = 0
identity x@y = 0

wildcard _ = 0

string "hello" = 0
string """hello""" = 0

char 'a' = 0

boolean true = 0
boolean false = 0

integer 1 = 0
number 1.0 = 0

array [] = 0
array [x] = 0
array [x, y] = 0

record {} = 0
record {x} = 0
record {x, y} = 0
record {x: y, y: x} = 0
record {a, b: c, d, e: f} = 0

parenthesized (x) = 0
parenthesized (x + y) = 0
parenthesized (((x))) = 0
