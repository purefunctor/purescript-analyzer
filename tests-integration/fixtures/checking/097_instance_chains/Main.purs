module Main where

import Prim.Boolean (True, False)

data Proxy a = Proxy

class TypeEq a b r | a b -> r

instance TypeEq a a True
else instance TypeEq a b False

testSame :: forall r. TypeEq Int Int r => Proxy r
testSame = Proxy

testDiff :: forall r. TypeEq Int String r => Proxy r
testDiff = Proxy

-- Force instantiation to verify resolved types
test = { testSame, testDiff }
