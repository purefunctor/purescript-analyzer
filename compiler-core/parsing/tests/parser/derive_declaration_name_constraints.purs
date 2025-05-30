module DeriveDeclarationNameConstraints where


derive instance functorParser :: Monoid Parser => Functor Parser
derive newtype instance functorParser :: Monoid Parser => Functor Parser

derive instance monoidTuple :: (Monoid a, Monoid b) => Monoid (Tuple a b)
derive newtype instance monoidTuple :: (Monoid a, Monoid b) => Monoid (Tuple a b)
