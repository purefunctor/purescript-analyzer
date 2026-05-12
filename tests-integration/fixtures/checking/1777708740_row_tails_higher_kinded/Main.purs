module Main where

foreign import data F :: Type -> Type
foreign import data G :: Type -> Type

foreign import data Box :: Row (Type -> Type) -> Type

foreign import unify :: forall r. Box r -> Box r -> Box r

foreign import withF :: forall r. Box ( f :: F | r )
foreign import withG :: forall r. Box ( g :: G | r )

test = unify withF withG
