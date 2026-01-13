export interface Example {
  id: string;
  title: string;
  description: string;
  category: string;
  source: string;
}

export const EXAMPLES: Example[] = [
  // Type-Level Programming
  {
    id: "row-union",
    title: "Row Union Solving",
    description: "Demonstrates bidirectional Row.Union constraint solving for extensible records.",
    category: "Type-Level Programming",
    source: `module Main where

import Prim.Row as Row

data Proxy :: forall k. k -> Type
data Proxy a = Proxy

deriveUnion :: forall u. Row.Union (a :: Int) (b :: String) u => Proxy u
deriveUnion = Proxy

deriveUnionLeft :: forall l. Row.Union l (b :: String) (a :: Int, b :: String) => Proxy l
deriveUnionLeft = Proxy

deriveUnionRight :: forall r. Row.Union (a :: Int) r (a :: Int, b :: String) => Proxy r
deriveUnionRight = Proxy

solveUnion = { deriveUnion, deriveUnionLeft, deriveUnionRight }
`,
  },
  {
    id: "row-cons",
    title: "Row.Cons Operations",
    description: "Shows how Row.Cons constructs and deconstructs row types at the type level.",
    category: "Type-Level Programming",
    source: `module Main where

import Prim.Row as Row

data Proxy :: forall k. k -> Type
data Proxy a = Proxy

-- Derive the full row from label, type, and tail
deriveCons :: forall row. Row.Cons "name" String () row => Proxy row
deriveCons = Proxy

-- Derive the tail from the full row
deriveTail :: forall tail. Row.Cons "name" String tail (name :: String, age :: Int) => Proxy tail
deriveTail = Proxy

-- Derive the field type from the full row
deriveType :: forall t. Row.Cons "name" t () (name :: String) => Proxy t
deriveType = Proxy

solveCons = { deriveCons, deriveTail, deriveType }
`,
  },
  {
    id: "rowlist",
    title: "RowToList Conversion",
    description: "Converts row types to type-level lists for type-level iteration.",
    category: "Type-Level Programming",
    source: `module Main where

import Prim.RowList as RL

data Proxy :: forall k. k -> Type
data Proxy a = Proxy

rowToListSimple :: forall list. RL.RowToList (a :: Int) list => Proxy list
rowToListSimple = Proxy

rowToListMultiple :: forall list. RL.RowToList (b :: String, a :: Int) list => Proxy list
rowToListMultiple = Proxy

rowToListEmpty :: forall list. RL.RowToList () list => Proxy list
rowToListEmpty = Proxy

solveRowToList = { rowToListSimple, rowToListMultiple, rowToListEmpty }
`,
  },
  {
    id: "symbol-ops",
    title: "Symbol Operations",
    description: "Type-level string operations: append, compare, and cons.",
    category: "Type-Level Programming",
    source: `module Main where

import Prim.Symbol (class Append, class Compare, class Cons)
import Prim.Ordering (Ordering, LT, EQ, GT)

data Proxy :: forall k. k -> Type
data Proxy a = Proxy

-- Append: Derive appended from left and right
deriveAppended :: forall appended. Append "Hello" "World" appended => Proxy appended
deriveAppended = Proxy

-- Append: Derive left from right and appended
deriveLeft :: forall left. Append left "World" "HelloWorld" => Proxy left
deriveLeft = Proxy

-- Compare symbols
compareLT :: forall ord. Compare "a" "b" ord => Proxy ord
compareLT = Proxy

compareEQ :: forall ord. Compare "hello" "hello" ord => Proxy ord
compareEQ = Proxy

-- Cons: Derive symbol from head and tail
deriveCons :: forall symbol. Cons "H" "ello" symbol => Proxy symbol
deriveCons = Proxy

forceSolve = { deriveAppended, deriveLeft, compareLT, compareEQ, deriveCons }
`,
  },
  {
    id: "int-ops",
    title: "Type-Level Integers",
    description: "Compile-time integer arithmetic: add, multiply, compare.",
    category: "Type-Level Programming",
    source: `module Main where

import Prim.Int (class Add, class Mul, class Compare, class ToString)
import Prim.Ordering (Ordering, LT, EQ, GT)

data Proxy :: forall k. k -> Type
data Proxy a = Proxy

-- Add: Derive sum from operands
deriveSum :: forall sum. Add 1 2 sum => Proxy sum
deriveSum = Proxy

-- Add: Derive right operand from left and sum
deriveRight :: forall right. Add 1 right 3 => Proxy right
deriveRight = Proxy

-- Mul: Derive product from operands
deriveMul :: forall product. Mul 3 4 product => Proxy product
deriveMul = Proxy

-- Compare integers
compareLT :: forall ord. Compare 1 2 ord => Proxy ord
compareLT = Proxy

compareGT :: forall ord. Compare 10 3 ord => Proxy ord
compareGT = Proxy

-- ToString: Convert integer to symbol
deriveString :: forall s. ToString 42 s => Proxy s
deriveString = Proxy

forceSolve = { deriveSum, deriveRight, deriveMul, compareLT, compareGT, deriveString }
`,
  },

  // Generic Deriving
  {
    id: "derive-generic",
    title: "Generic Deriving",
    description: "Derive Generic instances to get type-level representations of data types.",
    category: "Generic Deriving",
    source: `module Main where

import Data.Generic.Rep (class Generic)

data Void

data MyUnit = MyUnit

data Either a b = Left a | Right b

data Tuple a b = Tuple a b

newtype Wrapper a = Wrapper a

derive instance Generic Void _
derive instance Generic MyUnit _
derive instance Generic (Either a b) _
derive instance Generic (Tuple a b) _
derive instance Generic (Wrapper a) _

-- Use Proxy to force solving and emit Rep types
data Proxy a = Proxy

getVoid :: forall rep. Generic Void rep => Proxy rep
getVoid = Proxy

getMyUnit :: forall rep. Generic MyUnit rep => Proxy rep
getMyUnit = Proxy

getEither :: forall a b rep. Generic (Either a b) rep => Proxy rep
getEither = Proxy

forceSolve = { getVoid, getMyUnit, getEither }
`,
  },
  {
    id: "derive-newtype",
    title: "Newtype Deriving",
    description: "Derive Newtype instances for wrapper types to enable coercions.",
    category: "Generic Deriving",
    source: `module Main where

import Data.Newtype (class Newtype)

newtype UserId = UserId Int

newtype Email = Email String

newtype Wrapper a = Wrapper a

derive instance Newtype UserId _
derive instance Newtype Email _
derive instance Newtype (Wrapper a) _
`,
  },

  // Type Classes
  {
    id: "class-functor",
    title: "Functor Class",
    description: "Higher-kinded type class for mappable containers.",
    category: "Type Classes",
    source: `module Main where

class Functor f where
  map :: forall a b. (a -> b) -> f a -> f b

data Maybe a = Nothing | Just a

instance Functor Maybe where
  map _ Nothing = Nothing
  map f (Just a) = Just (f a)

data List a = Nil | Cons a (List a)

instance Functor List where
  map _ Nil = Nil
  map f (Cons x xs) = Cons (f x) (map f xs)
`,
  },
  {
    id: "fundep",
    title: "Functional Dependencies",
    description: "Use functional dependencies to guide type inference.",
    category: "Type Classes",
    source: `module Main where

-- Class with functional dependency: knowing 'a' determines 'b'
class Convert a b | a -> b where
  convert :: a -> b

instance Convert Int String where
  convert _ = "int"

instance Convert Boolean String where
  convert _ = "bool"

-- The fundep allows inferring the return type from the input type
testInt = convert 42
testBool = convert true
`,
  },
  {
    id: "instance-chains",
    title: "Instance Chains",
    description: "Use 'else' to create overlapping instances with fallback behavior.",
    category: "Type Classes",
    source: `module Main where

import Prim.Boolean (True, False)

data Proxy a = Proxy

class TypeEq a b r | a b -> r

instance TypeEq a a True
else instance TypeEq a b False

testSame :: forall r. TypeEq Int Int r => Proxy r
testSame = Proxy

testDiff :: forall r. TypeEq Int String r => Proxy r
testDiff = Proxy

-- Force instantiation to verify resolved types
test = { testSame, testDiff }
`,
  },
];

export const CATEGORIES = [...new Set(EXAMPLES.map((e) => e.category))];
