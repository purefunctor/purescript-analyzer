module Main where

import Safe.Coerce (coerce)

foreign import data Nominal :: Type -> Type

type role Nominal nominal

coerceNominalSame :: Nominal Int -> Nominal Int
coerceNominalSame = coerce

coerceNominalDifferent :: Nominal Int -> Nominal String
coerceNominalDifferent = coerce
