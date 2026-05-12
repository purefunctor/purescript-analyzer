module Main where

foreign import data Render :: Type -> Type -> Type -> Type

data Unit = Unit

foreign import data Start :: Type

data Use1 :: Type -> Type
data Use1 hooks

data Use2 :: Type -> Type
data Use2 hooks

foreign import pure :: forall hooks a. a -> Render hooks hooks a

foreign import discard ::
  forall a b x y z.
  Render x y a ->
  (a -> Render y z b) ->
  Render x z b

foreign import use1 :: forall hooks. Render hooks (Use1 hooks) Unit

foreign import use2 :: forall hooks. Render hooks (Use2 hooks) Unit

test :: Render Start (Use2 (Use1 Start)) Unit
test = do
  use1
  use2
  pure Unit
