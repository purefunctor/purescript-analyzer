module ClassDeclarationFull where

class TypeEquals b a <= TypeEquals a b | a -> b, b -> a where
  proof :: a -> b

class (TypeEquals a b, TypeEquals b c) <= TypeEquals a c | a b -> c where
  proof :: a -> c
