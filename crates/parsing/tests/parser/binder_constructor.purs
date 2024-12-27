module BinderConstructor where

constructor (Cons x xs) = 0
constructor Nil = 0

constructor (List.Cons x xs) = 0
constructor List.Nil = 0
