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
foreign import data Blob :: AttachmentKind
data KeysKind
foreign import data NoKeys :: KeysKind

foreign import data O :: Type -> CodecsArgsKind -> Type
foreign import data Opts :: Type
foreign import data Res :: Type -> Type
foreign import data StubAttachment :: Type
foreign import data Base64Attachment :: Type
foreign import data BlobAttachment :: Type

class IntOf :: forall k. k -> Int -> Constraint
class IntOf k int | k -> int
instance IntOf NoDoc 0
instance IntOf Stub 0
instance IntOf Base64 1
instance IntOf Blob 2
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

class IsJAttachmentCodec :: AttachmentKind -> Type -> Constraint
class IsJAttachmentCodec dk attachment | dk -> attachment
instance IsJAttachmentCodec Stub StubAttachment
instance IsJAttachmentCodec Base64 Base64Attachment
instance IsJAttachmentCodec Blob BlobAttachment

class IsCodec :: CodecsArgsKind -> Type -> Constraint
class IsCodec dak a | dak -> a
instance IsJAttachmentCodec a attachment => IsCodec (CA (Doc a) NoKeys) (Res attachment)

data BuildO = BuildO

class FoldingWithIndex f i x y z | f i x y -> z
class HFoldlWithIndex f x a b | f x a -> b

instance
  PickMax dk1 (Doc Base64) dk2 =>
  FoldingWithIndex BuildO (Proxy "attachments") (O Opts (CA dk1 kk)) (Proxy True) (O Opts (CA dk2 kk))

instance
  PickMax dk1 (Doc Blob) dk2 =>
  FoldingWithIndex BuildO (Proxy "binary") (O Opts (CA dk1 kk)) (Proxy True) (O Opts (CA dk2 kk))

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
  , FoldlRecord BuildO (O Opts (CA NoDoc NoKeys)) rl r (O Opts args)
  ) =>
  Build { | r } args

allDocs :: forall config args a. Build config args => IsCodec args a => config -> Proxy a
allDocs _ = Proxy

test :: Proxy (Res BlobAttachment)
test = allDocs { includeDocs: Proxy @True, attachments: Proxy @True, binary: Proxy @True }

testInferred = allDocs { includeDocs: Proxy @True, attachments: Proxy @True, binary: Proxy @True }
