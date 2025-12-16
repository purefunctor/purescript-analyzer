module BinderConstructorNestedContainers where

test a = case a of
  [Nothing, Nothing] -> 0
  [Just x, Nothing] -> x
  { a: Nothing, b: Just x } -> x
  Tuple [Nothing] { x: Nothing } -> 0
