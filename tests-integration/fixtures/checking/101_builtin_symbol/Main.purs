module Main where

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

-- Append: Derive right from left and appended
deriveRight :: forall right. Append "Hello" right "HelloWorld" => Proxy right
deriveRight = Proxy

-- Append: Empty strings
appendEmpty :: forall result. Append "" "test" result => Proxy result
appendEmpty = Proxy

appendEmptyRight :: forall result. Append "test" "" result => Proxy result
appendEmptyRight = Proxy

-- Compare: Less than
compareLT :: forall ord. Compare "a" "b" ord => Proxy ord
compareLT = Proxy

-- Compare: Equal
compareEQ :: forall ord. Compare "hello" "hello" ord => Proxy ord
compareEQ = Proxy

-- Compare: Greater than
compareGT :: forall ord. Compare "z" "a" ord => Proxy ord
compareGT = Proxy

-- Compare: Prefix comparison
comparePrefix :: forall ord. Compare "ab" "abc" ord => Proxy ord
comparePrefix = Proxy

-- Cons: Derive symbol from head and tail
deriveCons :: forall symbol. Cons "H" "ello" symbol => Proxy symbol
deriveCons = Proxy

-- Cons: Derive head and tail from symbol
deriveHeadTail :: forall head tail. Cons head tail "World" => Proxy head
deriveHeadTail = Proxy

-- Cons: Empty tail
consEmptyTail :: forall symbol. Cons "X" "" symbol => Proxy symbol
consEmptyTail = Proxy

-- Cons: Derive from single char symbol
consSingleChar :: forall head tail. Cons head tail "A" => Proxy tail
consSingleChar = Proxy

-- Force the solver to resolve all constraints
forceSolve =
  { deriveAppended
  , deriveLeft
  , deriveRight
  , appendEmpty
  , appendEmptyRight
  , compareLT
  , compareEQ
  , compareGT
  , comparePrefix
  , deriveCons
  , deriveHeadTail
  , consEmptyTail
  , consSingleChar
  }
