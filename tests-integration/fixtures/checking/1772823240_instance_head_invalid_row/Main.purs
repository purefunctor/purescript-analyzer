module Main where

data Proxy :: forall k. k -> Type
data Proxy a = Proxy

class T :: forall k. k -> Type
class T a

instance T ( a :: Int )
instance T { a :: Int }
instance T (Proxy ( a :: Int ))
instance T (Proxy { a :: Int })
