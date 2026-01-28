module Main where

import Type.Proxy (Proxy(..))

class Identity a where
  identity :: a -> a

instance Identity (Undefined a) where
  identity a =
    let
      proxy :: Proxy a
      proxy = Proxy
    in
      a
