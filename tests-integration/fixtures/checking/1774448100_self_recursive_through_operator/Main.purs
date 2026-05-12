module Main where

data List :: Type -> Type
data List a = Nil | Cons (List a)

append :: forall a. List a -> List Int -> List a
append = case _, _ of
  Nil, Nil -> Nil
  Cons p, Nil -> Cons (p <+> Nil)
  Nil, Cons p -> Cons (Nil <+> p)
  Cons p, Cons r -> Cons (p <+> r)

infixl 6 append as <+>
