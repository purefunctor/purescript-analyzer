module Data.Generic.Rep where

class Generic a rep | a -> rep where
  to :: rep -> a
  from :: a -> rep

data NoConstructors

data Constructor (name :: Symbol) a = Constructor a

data Sum a b = Inl a | Inr b

data Product a b = Product a b

data NoArguments = NoArguments

newtype Argument a = Argument a
