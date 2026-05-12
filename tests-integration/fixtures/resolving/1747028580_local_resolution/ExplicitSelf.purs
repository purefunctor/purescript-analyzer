module ExplicitSelf (module ExplicitSelf) where

value = 123

type Synonym = Int

data Maybe a = Just a | Nothing

newtype Id a = Id Int

class Eq a where
  eq :: a -> a -> Boolean

instance eqMaybe :: Maybe a => Eq (Maybe a) where
  eq _ _ = true

derive newtype instance eqId :: Eq a => Eq (Id a)

data HiddenConstructor = HiddenConstructor

data HiddenType = HiddenType
