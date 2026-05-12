module Main where

import Prim.Int (class Add, class ToString)
import Prim.Row (class Cons)
import Prim.Symbol (class Append)

data Proxy :: forall k. k -> Type
data Proxy a = Proxy

class Build n r | n -> r

instance Build 0 ()
else instance
  ( Add minusOne 1 currentId
  , ToString currentId labelId
  , Append "n" labelId actualLabel
  , Build minusOne minusOneResult
  , Cons actualLabel currentId minusOneResult finalResult
  ) => Build currentId finalResult

build :: forall n r. Build n r => Proxy n -> Proxy r
build _ = Proxy

test = build (Proxy :: Proxy 10000)
