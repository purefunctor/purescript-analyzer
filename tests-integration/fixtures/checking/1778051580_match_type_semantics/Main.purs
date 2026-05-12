module Main where

class IsEq a b o | a b -> o where
  isEq :: a -> b -> o

data Unit = Unit
data Output = Output

instance IsEq a a Output where
  isEq _ _ = Output
else instance IsEq a b Output where
  isEq _ _ = Output

testEqConcrete :: Output
testEqConcrete = isEq Unit Unit

testSkolemRight :: forall x. x -> Output
testSkolemRight x = isEq x Unit

testSkolemLeft :: forall x. x -> Output
testSkolemLeft x = isEq Unit x

testSkolemIsEq :: forall x y. x -> y -> Output
testSkolemIsEq x y = isEq y x

class TypeEquals :: forall k. k -> k -> Constraint
class TypeEquals a b | a -> b, b -> a

instance TypeEquals a a

typeEquals :: forall @a @b. TypeEquals a b => Unit
typeEquals = Unit

testOpenRows :: Unit
testOpenRows = typeEquals @{ params :: Unit | _ } @{ query :: Unit | _ }

class RowSame :: forall k. k -> k -> Constraint
class RowSame a b where
  rowSame :: Unit

instance RowSame a a where
  rowSame = Unit

testOpenRowsNoFd :: Unit
testOpenRowsNoFd = rowSame @{ params :: Unit | _ } @{ query :: Unit | _ }
