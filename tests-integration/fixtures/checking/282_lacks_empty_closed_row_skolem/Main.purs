module Main where

import Prim.Row as Row
import Type.Proxy (Proxy(..))

useLacksEmpty :: forall name. Row.Lacks name () => Proxy name -> Int
useLacksEmpty _ = 0

forwardLacksEmpty :: forall name. Proxy name -> Int
forwardLacksEmpty p = useLacksEmpty p
