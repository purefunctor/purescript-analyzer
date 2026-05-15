module Main where

import Prim.Int (class Compare)
import Prim.Ordering (LT)
import Prim.Boolean (False, True)
import Type.Proxy (Proxy(..))

foreign import data TypeExpr :: forall k. k -> Type

foreign import data Eq :: forall a. a -> a -> TypeExpr Boolean

infix 4 type Eq as ==

class Eval :: forall k. TypeExpr k -> k -> Constraint
class Eval expr result | expr -> result

instance Eval (Eq a a) True
else instance Eval (Eq a b) False

class If :: forall k. Boolean -> k -> k -> k -> Constraint
class If bool onTrue onFalse output | bool onTrue onFalse -> output
instance If True onTrue onFalse onTrue
else instance If False onTrue onFalse onFalse

foreign import data NoDoc :: Type
foreign import data Doc :: Type -> Type
foreign import data Stub :: Type
foreign import data Base64 :: Type

class IntOf :: forall k. k -> Int -> Constraint
class IntOf k int | k -> int

instance IntOf NoDoc 0
instance IntOf (Doc Stub) 1
instance IntOf (Doc Base64) 2

class PickMax :: forall k. k -> k -> k -> Constraint
class PickMax a b c | a b -> c

instance
  ( IntOf a ia
  , IntOf b ib
  , Compare ia ib ord
  , Eval (LT == ord) isLess
  , If isLess b a c
  ) =>
  PickMax a b c

pickMax :: forall a b c. PickMax a b c => Proxy a -> Proxy b -> Proxy c
pickMax _ _ = Proxy

test :: Proxy (Doc Base64)
test = pickMax (Proxy :: Proxy (Doc Base64)) (Proxy :: Proxy (Doc Stub))

testInferred = pickMax (Proxy :: Proxy (Doc Base64)) (Proxy :: Proxy (Doc Stub))
