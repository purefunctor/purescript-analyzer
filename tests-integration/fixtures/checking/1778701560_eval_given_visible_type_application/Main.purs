module Main where

import Prim.Boolean (False, True)
import Type.Proxy (Proxy(..))

foreign import data TypeExpr :: forall k. k -> Type
foreign import data NotEq :: Boolean -> Boolean -> TypeExpr Boolean

class Eval :: forall k. TypeExpr k -> k -> Constraint
class Eval expr result | expr -> result

instance Eval (NotEq a a) False
else instance Eval (NotEq a b) True

class IsBoolean :: Boolean -> Constraint
class IsBoolean bool

instance IsBoolean True
instance IsBoolean False

foreign import reflectBoolean :: forall bool. IsBoolean bool => Proxy bool -> Boolean

test :: forall x. IsBoolean x => Eval (NotEq False True) x => Boolean
test = reflectBoolean (Proxy @x)
