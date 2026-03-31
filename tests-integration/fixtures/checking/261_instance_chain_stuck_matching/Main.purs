module Main where

import Effect.Aff (Aff)

data Unit = Unit

data Spec :: (Type -> Type) -> Type
data Spec g = Spec

class Convert :: Type -> (Type -> Type) -> Constraint
class Convert t m | t -> m

instance convertFunction :: Convert (a -> m Unit) m
else instance convertAction :: Convert (m Unit) m

it :: forall t g. Convert t g => String -> t -> Spec g
it _ _ = Spec

foreign import action :: forall m. m Unit

test :: Spec Aff
test = it "do" do action

test2 :: Spec Aff
test2 = it "function" \(_ :: Int) -> action

test3 :: Spec Aff
test3 = it "variable" action
