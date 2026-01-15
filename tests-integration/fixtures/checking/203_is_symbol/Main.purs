module Main where

import Type.Proxy (Proxy(..))
import Data.Symbol (class IsSymbol, reflectSymbol)

test :: String
test = reflectSymbol (Proxy :: Proxy "hello")

test' = reflectSymbol (Proxy :: Proxy "hello")

testEmpty :: String
testEmpty = reflectSymbol (Proxy :: Proxy "")

testEmpty' = reflectSymbol (Proxy :: Proxy "")
