module Main where

import Prim.Row as Row
import Type.Proxy (Proxy(..))

type RowApply :: forall k. (Row k -> Row k) -> Row k -> Row k
type RowApply f a = f a

infixr 0 type RowApply as +

type LeftRow :: Row Type -> Row Type
type LeftRow r = ( left :: Boolean | r )

type RightRow :: Row Type -> Row Type
type RightRow r = ( right :: String | r )

type CombinedRow :: Row Type -> Row Type
type CombinedRow r = LeftRow + RightRow + r

mk
  :: forall a r r' s
   . Row.Cons s a r' (CombinedRow r)
  => Proxy s
  -> a
  -> Proxy (CombinedRow r)
mk _ _ = Proxy

test :: forall r. Proxy (CombinedRow r)
test = mk (Proxy :: _ "right") ""

forceSolve =
  { test }
