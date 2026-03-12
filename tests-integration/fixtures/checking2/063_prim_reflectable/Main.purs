module Main where

import Type.Proxy (Proxy(..))
import Data.Reflectable (class Reflectable, reflectType)
import Data.Ordering (Ordering)
import Prim.Boolean (True, False)
import Prim.Ordering (LT, EQ, GT)

testSymbol :: String
testSymbol = reflectType (Proxy :: Proxy "hello")

testSymbol' = reflectType (Proxy :: Proxy "hello")

testInt :: Int
testInt = reflectType (Proxy :: Proxy 42)

testInt' = reflectType (Proxy :: Proxy 42)

testTrue :: Boolean
testTrue = reflectType (Proxy :: Proxy True)

testTrue' = reflectType (Proxy :: Proxy True)

testFalse :: Boolean
testFalse = reflectType (Proxy :: Proxy False)

testFalse' = reflectType (Proxy :: Proxy False)

testLT :: Ordering
testLT = reflectType (Proxy :: Proxy LT)

testLT' = reflectType (Proxy :: Proxy LT)

testEQ :: Ordering
testEQ = reflectType (Proxy :: Proxy EQ)

testEQ' = reflectType (Proxy :: Proxy EQ)

testGT :: Ordering
testGT = reflectType (Proxy :: Proxy GT)

testGT' = reflectType (Proxy :: Proxy GT)
