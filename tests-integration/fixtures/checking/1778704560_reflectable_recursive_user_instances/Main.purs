module Main where

import Data.Reflectable (class Reflectable, reflectType)
import Type.Proxy (Proxy(..))

foreign import data ItemA :: Type

foreign import data ListTL :: Type
foreign import data NilTL :: ListTL
foreign import data ConsTL :: Type -> ListTL -> ListTL

data Item = ItemA

data Items = NilItems | ConsItems Item Items

instance Reflectable ItemA Item where
  reflectType _ = ItemA

instance Reflectable NilTL Items where
  reflectType _ = NilItems

instance (Reflectable head Item, Reflectable tail Items) => Reflectable (ConsTL head tail) Items where
  reflectType _ = ConsItems (reflectType (Proxy :: Proxy head)) (reflectType (Proxy :: Proxy tail))

test :: Items
test = reflectType (Proxy :: Proxy (ConsTL ItemA (ConsTL ItemA NilTL)))

test' = reflectType (Proxy :: Proxy (ConsTL ItemA (ConsTL ItemA NilTL)))
