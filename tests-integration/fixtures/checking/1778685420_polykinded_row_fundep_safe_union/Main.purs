module Main where

import Prim.Row as Row
import Prim.RowList (RowList)
import Prim.RowList as RL

foreign import data Run :: Row (Type -> Type) -> Type -> Type

type NaturalTransformation f g = forall a. f a -> g a
infixr 4 type NaturalTransformation as ~>

data Unit = Unit

unit :: Unit
unit = Unit

foreign import data Effect :: Type -> Type
foreign import data State :: Type -> Type -> Type
foreign import data Reader :: Type -> Type -> Type

foreign import pure :: forall r a. a -> Run r a

foreign import bindRun :: forall r a b. Run r a -> (a -> Run r b) -> Run r b

foreign import unsafeCoerce :: forall a b. a -> b

type RowApply :: forall k. (Row k -> Row k) -> Row k -> Row k
type RowApply f a = f a

infixr 0 type RowApply as +

type EFFECT :: Row (Type -> Type) -> Row (Type -> Type)
type EFFECT r = ( effect :: Effect | r )

type STATE :: Type -> Row (Type -> Type) -> Row (Type -> Type)
type STATE s r = ( state :: State s | r )

type READER :: Type -> Row (Type -> Type) -> Row (Type -> Type)
type READER a r = ( reader :: Reader a | r )

class SafeUnion :: forall k. Row k -> Row k -> Row k -> Constraint
class SafeUnion a b c | a b -> c

foreign import data TypeExpr :: Type -> Type

class Eval :: forall k. TypeExpr k -> k -> Constraint
class Eval expr result | expr -> result

foreign import data FromRow :: forall k. Row k -> TypeExpr (RowList k)

instance RL.RowToList row list => Eval (FromRow row) list

class ListToRow :: forall k. RowList k -> Row k -> Constraint
class ListToRow list row | list -> row

instance ListToRow RL.Nil ()
instance (Row.Cons name typ rowTail row, ListToRow tail rowTail) => ListToRow (RL.Cons name typ tail) row

foreign import data ToRow :: forall k. RowList k -> TypeExpr (Row k)

instance ListToRow list row => Eval (ToRow list) row

foreign import data Union :: forall k. Row k -> Row k -> TypeExpr (Row k)

instance Row.Union left right union => Eval (Union left right) union

foreign import data Nub :: forall k. Row k -> TypeExpr (Row k)

instance Row.Nub original nubbed => Eval (Nub original) nubbed

foreign import data UnionAndNubRowLists :: forall k. RowList k -> RowList k -> TypeExpr (RowList k)

instance
  ( Eval (ToRow a) aRow
  , Eval (ToRow b) bRow
  , Eval (Union aRow bRow) union
  , Eval (Nub union) cRow
  , Eval (FromRow cRow) c
  ) =>
  Eval (UnionAndNubRowLists a b) c

instance
  ( Eval (FromRow r1) rl1
  , Eval (FromRow r2) rl2
  , Eval (UnionAndNubRowLists rl1 rl2) rlResult
  , Eval (ToRow rlResult) result
  ) =>
  SafeUnion r1 r2 result

expand :: forall r1 @r2 r3. SafeUnion r1 r2 r3 => Run r1 ~> Run r3
expand = unsafeCoerce

composeFlipped
  :: forall r1 r2 r3 a b c
   . SafeUnion r1 r2 r3
  => SafeUnion r2 r1 r3
  => (a -> Run r1 b)
  -> (b -> Run r2 c)
  -> (a -> Run r3 c)
composeFlipped f g a =
  bindRun (expand @r2 (f a)) \b -> expand @r1 (g b)

infixr 1 composeFlipped as >+>

computation1 :: String -> Run (EFFECT ()) Int
computation1 _ = pure 0

computation2 :: Int -> Run (EFFECT + STATE String + ()) String
computation2 _ = pure ""

computation3 :: String -> Run (STATE String + READER Boolean + ()) Unit
computation3 _ = pure unit

composeFlippedThree :: String -> Run (EFFECT + STATE String + READER Boolean + ()) Unit
composeFlippedThree = computation1 >+> computation2 >+> computation3
