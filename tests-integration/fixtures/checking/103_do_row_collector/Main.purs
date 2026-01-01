module Main where

import Prim.Row as Row
import Prim.Int as Int
import Prim.Symbol as Symbol

-- An accumulating indexed monad that collects bound value types into a row.
-- Tracks: field count (type-level Int), accumulated row, and current value type.
foreign import data Collector :: Int -> Row Type -> Type -> Type

-- Counts combine (countL + countR + 1), field name derived from countOut.
-- Input row is not carried forward; only its count offsets field numbering.
foreign import bind
  :: forall countL countR countLR countOut fieldNum fieldName a b r r' s
   . Int.Add countL countR countLR
  => Int.Add countLR 1 countOut
  => Int.ToString countOut fieldNum
  => Symbol.Append "_" fieldNum fieldName
  => Row.Cons fieldName a r r'
  => Collector countL s a -> (a -> Collector countR r b) -> Collector countOut r' b

foreign import discard
  :: forall countL countR countLR countOut fieldNum fieldName a b r r' s
   . Int.Add countL countR countLR
  => Int.Add countLR 1 countOut
  => Int.ToString countOut fieldNum
  => Symbol.Append "_" fieldNum fieldName
  => Row.Cons fieldName a r r'
  => Collector countL s a -> (a -> Collector countR r b) -> Collector countOut r' b

foreign import pure :: forall a. a -> Collector 0 () a

test1 = do
  x <- pure 0
  y <- pure ""
  z <- pure 'a'
  pure { x, y, z }

test2 = do
  x <- pure 'a'
  y <- pure true
  pure { x, y }

test3 = do
  x <- pure 0
  let y = x
  z <- pure ""
  pure { y, z }

-- Compose collectors; field names stay unique due to count accumulation
test4 = do
  a <- test1
  b <- test2
  c <- test3
  pure { a, b, c }
