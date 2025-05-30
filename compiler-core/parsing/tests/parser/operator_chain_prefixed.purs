module OperatorChainPrefixed where

expression = 1 L.: 2 L.: L.Nil

binder (a L.: b L.: L.Nil) = a S.+ b

type Type = Integer L.: Integer L.: L.Nil
