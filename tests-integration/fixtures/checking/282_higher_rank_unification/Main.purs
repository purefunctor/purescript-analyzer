module Main where

import Lib (Maybe(..), isJust, Fn2, Fn3, runFn2, runFn3)

foreign import findImpl
  :: forall a b. Fn2 (forall c. Maybe c) (a -> Maybe b) (Maybe b)

findMap :: forall a b. (a -> Maybe b) -> Maybe b
findMap = runFn2 findImpl Nothing

foreign import findMapImpl
  :: forall a b
   . Fn3 (forall c. Maybe c) (forall c. Maybe c -> Boolean) (a -> Maybe b) (Maybe b)

findMap' :: forall a b. (a -> Maybe b) -> Maybe b
findMap' = runFn3 findMapImpl Nothing isJust
