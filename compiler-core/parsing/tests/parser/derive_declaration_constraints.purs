module DeriveDeclarationConstraints where

derive instance Monoid Parser => Functor Parser
derive newtype instance Monoid Parser => Functor Parser

derive instance (Monoid a, Monoid b) => Monoid (Tuple a b)
derive newtype instance (Monoid a, Monoid b) => Monoid (Tuple a b)
