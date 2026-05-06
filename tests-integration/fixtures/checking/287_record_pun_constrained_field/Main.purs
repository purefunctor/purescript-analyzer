module Main where

class Capability :: (Type -> Type) -> Constraint
class Capability m

foreign import fetch :: forall m. Capability m => Int -> m Int

type Setup =
  { fetch :: forall m. Capability m => Int -> m Int
  }

test :: Setup
test = { fetch }
