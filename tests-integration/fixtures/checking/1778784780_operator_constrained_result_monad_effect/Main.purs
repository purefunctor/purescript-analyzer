module Main where

infixr 0 apply as $

apply :: forall a b. (a -> b) -> a -> b
apply f x = f x

data Proxy :: forall k. k -> Type
data Proxy a = Proxy

class TypeEquals :: forall k. k -> k -> Constraint
class TypeEquals a b | a -> b, b -> a

instance TypeEquals a a

foreign import data Effect :: Type -> Type

foreign import data Run :: Row (Type -> Type) -> Type -> Type

type EFFECT r = (effect :: Effect | r)

class MonadEffect m

instance TypeEquals (Proxy r1) (Proxy (EFFECT r2)) => MonadEffect (Run r1)

data Initial = Initial

data Env a = Env

foreign import envGeneric :: forall a m. a -> MonadEffect m => m (Env a)

testDirect :: forall r. Run (EFFECT r) (Env Initial)
testDirect = envGeneric Initial

testDollar :: forall r. Run (EFFECT r) (Env Initial)
testDollar = envGeneric $ Initial
