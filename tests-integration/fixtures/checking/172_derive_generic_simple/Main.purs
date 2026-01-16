module Main where

import Data.Generic.Rep (class Generic)

data Void

data MyUnit = MyUnit

data Identity a = Identity a

data Either a b = Left a | Right b

data Tuple a b = Tuple a b

newtype Wrapper a = Wrapper a

derive instance Generic Void _
derive instance Generic MyUnit _
derive instance Generic (Identity a) _
derive instance Generic (Either a b) _
derive instance Generic (Tuple a b) _
derive instance Generic (Wrapper a) _

-- Use forceSolve to emit the Rep types in the snapshot
data Proxy a = Proxy

getVoid :: forall rep. Generic Void rep => Proxy rep
getVoid = Proxy

getMyUnit :: forall rep. Generic MyUnit rep => Proxy rep
getMyUnit = Proxy

getIdentity :: forall a rep. Generic (Identity a) rep => Proxy rep
getIdentity = Proxy

getEither :: forall a b rep. Generic (Either a b) rep => Proxy rep
getEither = Proxy

getTuple :: forall a b rep. Generic (Tuple a b) rep => Proxy rep
getTuple = Proxy

getWrapper :: forall a rep. Generic (Wrapper a) rep => Proxy rep
getWrapper = Proxy

forceSolve = { getVoid, getMyUnit, getIdentity, getEither, getTuple, getWrapper }
