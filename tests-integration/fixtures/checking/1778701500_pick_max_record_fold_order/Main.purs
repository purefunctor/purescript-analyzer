module Main where

import Prim.Int (class Add, class Compare)
import Prim.Ordering (LT)
import Prim.Row as Row
import Prim.RowList as RL
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

data CodecsArgsKind
foreign import data CA :: DocKind -> KeysKind -> CodecsArgsKind
data DocKind
foreign import data NoDoc :: DocKind
foreign import data Doc :: AttachmentKind -> DocKind
data AttachmentKind
foreign import data Stub :: AttachmentKind
foreign import data Base64 :: AttachmentKind
data KeysKind
foreign import data NoKeys :: KeysKind

foreign import data O :: Type -> CodecsArgsKind -> Type
foreign import data Opts :: Type

class IntOf :: forall k. k -> Int -> Constraint
class IntOf k int | k -> int
instance IntOf NoDoc 0
instance IntOf Stub 0
instance IntOf Base64 1
instance (IntOf dk i1, Add 1 i1 i2) => IntOf (Doc dk) i2

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

data BuildO = BuildO

class FoldingWithIndex f i x y z | f i x y -> z
class HFoldlWithIndex f x a b | f x a -> b

instance
  PickMax dk1 (Doc Base64) dk2 =>
  FoldingWithIndex BuildO (Proxy "attachments") (O Opts (CA dk1 kk)) (Proxy True) (O Opts (CA dk2 kk))

instance
  PickMax dk1 (Doc Stub) dk2 =>
  FoldingWithIndex BuildO (Proxy "includeDocs") (O Opts (CA dk1 kk)) (Proxy True) (O Opts (CA dk2 kk))

instance
  ( FoldingWithIndex f (Proxy sym) x (Proxy y) z
  , HFoldlWithIndex f z (Proxy rl) b
  ) =>
  HFoldlWithIndex f x (Proxy (RL.Cons sym y rl)) b

instance HFoldlWithIndex f x (Proxy RL.Nil) x

class FoldlRecord f x (rl :: RL.RowList Type) (r :: Row Type) b | f x rl -> b, rl -> r
instance
  ( Row.Cons sym a r' r
  , FoldingWithIndex f (Proxy sym) x a z
  , FoldlRecord f z rl r b
  ) =>
  FoldlRecord f x (RL.Cons sym a rl) r b
instance FoldlRecord f x RL.Nil r x

class Build config args | config -> args
instance
  ( RL.RowToList r rl
  , FoldlRecord BuildO (O Opts (CA NoDoc NoKeys)) rl r args
  ) =>
  Build { | r } args

build :: forall config args. Build config args => config -> Proxy args
build _ = Proxy

test :: Proxy (O Opts (CA (Doc Base64) NoKeys))
test = build { includeDocs: Proxy @True, attachments: Proxy @True }

testInferred = build { includeDocs: Proxy @True, attachments: Proxy @True }
