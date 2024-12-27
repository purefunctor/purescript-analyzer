module Expression where

typed = 1 :: T

tick = 1 `plus` 2 `plus` 3

tick = 1 `2 + 3` 4 `5 + 6` 7

negate = -1

negate = -x

application = f @T t

if_then_else = if x then y else z

prefixed = Data.List.head
prefixed = Data.List.Cons
prefixed = Data.List.(:)

name = head
name = Cons
name = (:)

section = _
hole = ?hole
string = "hello"
char = 'c'
boolean = true
boolean = false
number = 1.0

let_in = let x = y in z

let_in =
  let x = y
  in z

let_in =
  let
    x = y
  in
    z

let_in =
  let
    life :: Int
    life = 42
  in
    life
