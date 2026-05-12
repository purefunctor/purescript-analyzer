module Main where

type RowApply :: forall k. (Row k -> Row k) -> Row k -> Row k
type RowApply f a = f a

infixr 6 type RowApply as +

type IncludeX r = ( x :: Int | r )
type IncludeY r = ( y :: String | r )
type IncludeZ r = ( z :: Boolean | r )
type Include = IncludeX + IncludeY + IncludeZ + ()

class TypeEquals :: forall k. k -> k -> Constraint
class TypeEquals a b | a -> b, b -> a

instance TypeEquals a a

foreign import unsafeCoerce :: forall a b. a -> b

coerceRecord :: forall a b. TypeEquals a b => { | a } -> { | b }
coerceRecord = unsafeCoerce

transform :: forall a. TypeEquals a Include => { | a } -> { x :: Int, y :: String, z :: Boolean }
transform v = coerceRecord v

test = transform { x: 42, y: "life", z: false }
