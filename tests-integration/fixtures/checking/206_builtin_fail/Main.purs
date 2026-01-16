module Main where

import Prim.TypeError (class Fail, Text, Quote, Beside, Above)

data Proxy :: forall k. k -> Type
data Proxy a = Proxy

failBasic :: forall a. Fail (Text "This operation is not allowed") => a -> a
failBasic x = x

useFailBasic :: Int
useFailBasic = failBasic 42

failComplex :: forall a. Fail (Above (Text "Error:") (Beside (Text "Type ") (Quote a))) => Proxy a -> Proxy a
failComplex p = p

useFailComplex :: Proxy String
useFailComplex = failComplex Proxy
