module Main where

import Data.Symbol (class IsSymbol, reflectSymbol)
import Prim.Symbol (class Append, class Compare, class Cons)
import Type.Proxy (Proxy(..))

deriveAppended :: forall appended. Append "Hello" "World" appended => Proxy appended
deriveAppended = Proxy

deriveLeft :: forall left. Append left "World" "HelloWorld" => Proxy left
deriveLeft = Proxy

deriveRight :: forall right. Append "Hello" right "HelloWorld" => Proxy right
deriveRight = Proxy

compareLT :: forall ord. Compare "a" "b" ord => Proxy ord
compareLT = Proxy

compareEQ :: forall ord. Compare "hello" "hello" ord => Proxy ord
compareEQ = Proxy

compareGT :: forall ord. Compare "z" "a" ord => Proxy ord
compareGT = Proxy

deriveCons :: forall symbol. Cons "H" "ello" symbol => Proxy symbol
deriveCons = Proxy

deriveHeadTail :: forall head tail. Cons head tail "World" => Proxy head
deriveHeadTail = Proxy

symbolValue :: String
symbolValue = reflectSymbol (Proxy :: Proxy "hello")

symbolValue' = reflectSymbol (Proxy :: Proxy "")

forceSolve =
  { deriveAppended
  , deriveLeft
  , deriveRight
  , compareLT
  , compareEQ
  , compareGT
  , deriveCons
  , deriveHeadTail
  , symbolValue
  , symbolValue'
  }
