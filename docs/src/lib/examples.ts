export interface Example {
  id: string;
  title: string;
  description: string;
  category: string;
  icon: string;
  source: string;
}

export const EXAMPLES: Example[] = [
  // Basics - proving compiler capabilities
  {
    id: "constraint-generalisation",
    title: "Constraint Generalisation",
    description: "Infer type class constraints from usage in untyped bindings.",
    category: "Basics",
    icon: "sparkles",
    source: `module Main where

class Functor f where
  map :: forall a b. (a -> b) -> f a -> f b

class Functor f <= Apply f where
  apply :: forall a b. f (a -> b) -> f a -> f b

class Apply m <= Bind m where
  bind :: forall a b. m a -> (a -> m b) -> m b

class Semigroup a where
  append :: a -> a -> a

-- No type signature: the compiler infers Functor f constraint
-- Hover to see: forall f a b. Functor f => (a -> b) -> f a -> f b
inferredMap f xs = map f xs

-- Infers Apply constraint from usage of apply
-- Hover to see: forall f a b. Apply f => f (a -> b) -> f a -> f b
inferredApply ff fa = apply ff fa

-- Infers multiple constraints: Bind m, Semigroup a
-- Hover to see: forall m a. Bind m => Semigroup a => m a -> m a -> m a
inferredBindAppend ma mb = bind ma (\\a -> map (append a) mb)
`,
  },
  {
    id: "instance-deriving",
    title: "Instance Deriving",
    description: "Derive Generic and Newtype instances for data declarations.",
    category: "Basics",
    icon: "wand",
    source: `module Main where

import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)

-- Algebraic data types derive Generic
data Maybe a = Nothing | Just a
data Either a b = Left a | Right b
data Tree a = Leaf | Branch (Tree a) a (Tree a)

derive instance Generic (Maybe a) _
derive instance Generic (Either a b) _
derive instance Generic (Tree a) _

-- Newtypes derive both Generic and Newtype
newtype UserId = UserId Int
newtype Email = Email String
newtype Wrapper a = Wrapper a

derive instance Generic UserId _
derive instance Generic Email _
derive instance Generic (Wrapper a) _

derive instance Newtype UserId _
derive instance Newtype Email _
derive instance Newtype (Wrapper a) _

-- Force Generic solving to see Rep types
data Proxy a = Proxy

getTreeRep :: forall a rep. Generic (Tree a) rep => Proxy rep
getTreeRep = Proxy

getMaybeRep :: forall a rep. Generic (Maybe a) rep => Proxy rep
getMaybeRep = Proxy

forceSolve = { getTreeRep, getMaybeRep }
`,
  },
  {
    id: "type-classes",
    title: "Type Classes",
    description: "Type classes, functional dependencies, and instance chains.",
    category: "Basics",
    icon: "layers",
    source: `module Main where

import Prim.Boolean (True, False)

data Proxy a = Proxy

-- Basic type class with functional dependency
-- Knowing 'a' determines 'b'
class Convert a b | a -> b where
  convert :: a -> b

instance Convert Int String where
  convert _ = "int"

instance Convert Boolean String where
  convert _ = "bool"

-- Fundep guides inference: no type annotation needed
testConvert = convert 42

-- Instance chains with 'else' for overlapping instances
class TypeEq a b (result :: Boolean) | a b -> result

instance TypeEq a a True
else instance TypeEq a b False

-- Multi-parameter class with two fundeps
class Combine a b c | a b -> c, c -> a b where
  combine :: a -> b -> c
  split :: c -> { fst :: a, snd :: b }

instance Combine Int String { int :: Int, str :: String } where
  combine i s = { int: i, str: s }
  split r = { fst: r.int, snd: r.str }

-- Force solving to verify inferred types
eqSame :: forall r. TypeEq Int Int r => Proxy r
eqSame = Proxy

eqDiff :: forall r. TypeEq Int String r => Proxy r
eqDiff = Proxy

test = { eqSame, eqDiff, testConvert }
`,
  },

  // Type-Level Programming
  {
    id: "row-union",
    title: "Row Union",
    description: "Bidirectional Row.Union constraint solving for extensible records.",
    category: "Type-Level Programming",
    icon: "merge",
    source: `module Main where

import Prim.Row as Row

data Proxy :: forall k. k -> Type
data Proxy a = Proxy

-- Derive the union from left and right
deriveUnion :: forall u. Row.Union (a :: Int) (b :: String) u => Proxy u
deriveUnion = Proxy

-- Derive the left row from right and union
deriveLeft :: forall l. Row.Union l (b :: String) (a :: Int, b :: String) => Proxy l
deriveLeft = Proxy

-- Derive the right row from left and union
deriveRight :: forall r. Row.Union (a :: Int) r (a :: Int, b :: String) => Proxy r
deriveRight = Proxy

-- Practical example: extensible record functions
merge :: forall left right union.
  Row.Union left right union =>
  Row.Nub union union =>
  Record left -> Record right -> Record union
merge l r = unsafeMerge l r

foreign import unsafeMerge :: forall a b c. a -> b -> c

test = merge { a: 1 } { b: "hello" }
`,
  },
  {
    id: "int-compare",
    title: "Comparison Proofs",
    description: "Type-level proofs of integer comparison transitivity and symmetry.",
    category: "Type-Level Programming",
    icon: "scale",
    source: `module Main where

import Prim.Int (class Compare)
import Prim.Ordering (LT, EQ, GT)

data Proxy :: forall k. k -> Type
data Proxy a = Proxy

-- Assertion helpers capture comparison results in row types
assertLT :: forall l r. Compare l r LT => Proxy (left :: l, right :: r)
assertLT = Proxy

assertGT :: forall l r. Compare l r GT => Proxy (left :: l, right :: r)
assertGT = Proxy

assertEQ :: forall l r. Compare l r EQ => Proxy (left :: l, right :: r)
assertEQ = Proxy

-- Symmetry: if m > n then n < m
symmLT :: forall m n. Compare m n GT => Proxy (left :: n, right :: m)
symmLT = assertLT

-- Reflexivity: n == n for any integer
reflEQ :: forall (n :: Int). Proxy (left :: n, right :: n)
reflEQ = assertEQ

-- Transitivity: if m < n and n < p, then m < p
transLT :: forall m n p.
  Compare m n LT =>
  Compare n p LT =>
  Proxy n -> Proxy (left :: m, right :: p)
transLT _ = assertLT

-- Concrete proof: 1 < 5 < 10 implies 1 < 10
proof1LT10 :: Proxy (left :: 1, right :: 10)
proof1LT10 = transLT (Proxy :: Proxy 5)
`,
  },
  {
    id: "recursive-constraints",
    title: "Recursive Constraints",
    description: "Build row types recursively using type-level integers and symbols.",
    category: "Type-Level Programming",
    icon: "loop",
    source: `module Main where

import Prim.Int (class Add, class ToString)
import Prim.Row (class Cons)
import Prim.Symbol (class Append)

data Proxy :: forall k. k -> Type
data Proxy a = Proxy

-- Recursively build a row type from an integer
-- Build 3 r => r ~ (n1 :: 1, n2 :: 2, n3 :: 3)
class Build n r | n -> r

instance Build 0 ()
else instance
  ( Add minusOne 1 currentId
  , ToString currentId labelId
  , Append "n" labelId actualLabel
  , Build minusOne minusOneResult
  , Cons actualLabel currentId minusOneResult finalResult
  ) => Build currentId finalResult

build :: forall n r. Build n r => Proxy n -> Proxy r
build _ = Proxy

-- Builds: (n1 :: 1, n2 :: 2, n3 :: 3, n4 :: 4, n5 :: 5)
test = build (Proxy :: Proxy 5)
`,
  },
];

export const CATEGORIES = [...new Set(EXAMPLES.map((e) => e.category))];
