module Main where

import Prim.Row as Row

foreign import unsafeCoerce :: forall a b. a -> b

union :: forall r1 r2 r3. Row.Union r1 r2 r3 => Record r1 -> Record r2 -> Record r3
union _ _ = unsafeCoerce {}

addField :: forall r. { a :: Int | r } -> { a :: Int, b :: String | r }
addField x = union x { b: "hi" }

test = addField { a: 1, c: true }

insertX :: forall r. Row.Lacks "x" r => Record r -> Record (x :: Int | r)
insertX _ = unsafeCoerce {}

insertOpen :: forall r. Row.Lacks "x" r => { a :: Int | r } -> { x :: Int, a :: Int | r }
insertOpen x = insertX x

test2 = insertOpen { a: 1, b: "hi" }
