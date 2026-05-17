module Main where

foreign import data Aff :: Type -> Type

newtype Z3 :: Type -> Type -> Type -> Type
newtype Z3 r mode a = Z3 (Aff a)

run :: forall a. (forall r mode. Z3 r mode a) -> Aff a
run (Z3 m) = m

newtype Box a = Box a

unbox :: (forall a. Box a) -> forall a. a
unbox (Box x) = x
