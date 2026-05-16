module Main where

foreign import data Aff :: Type -> Type
foreign import data Spec :: (Type -> Type) -> Type -> Type
foreign import data Unit :: Type
foreign import data Error :: Type

class MonadThrow e m
instance MonadThrow Error Aff

class Example t arg m | t -> arg, t -> m where
  evaluateExample :: t -> Unit

instance Example (arg -> m Unit) arg m where
  evaluateExample _ = unit

else instance Example (m Unit) Unit m where
  evaluateExample _ = unit

foreign import unit :: Unit
foreign import shouldEqual :: forall m. MonadThrow Error m => Int -> Int -> m Unit

it :: forall t arg g. Example t arg g => String -> t -> Spec g arg
it _ _ = spec

foreign import spec :: forall g a. Spec g a

test :: Spec Aff Int
test = it "returns correct total" \res -> shouldEqual res 4
