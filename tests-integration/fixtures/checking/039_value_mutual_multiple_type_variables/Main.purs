module Main where

loop y = chooseFirst y y

chooseFirst :: forall a b. a -> b -> a
chooseFirst x _ = loop x
