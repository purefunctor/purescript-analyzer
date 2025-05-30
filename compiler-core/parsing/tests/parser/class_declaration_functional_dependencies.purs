module ClassDeclarationFunctionalDependencies where

class TypeEquals a b | a -> b, b -> a where
  proof :: a -> b

class Determined a | -> a where
  determined :: a

class Empty a | -> a
