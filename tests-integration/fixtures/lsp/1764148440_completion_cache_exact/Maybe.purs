module Data.Maybe where

data Maybe a = Just a | Nothing

maybe :: forall a. a -> Maybe a
maybe = Just

fromMaybe :: forall a. a -> Maybe a -> a
fromMaybe def Nothing = def
fromMaybe _ (Just x) = x
