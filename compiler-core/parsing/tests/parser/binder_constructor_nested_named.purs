module BinderConstructorNestedNamed where

test a = case a of
  x@Nothing -> x
  x@(Just y) -> y
  Tuple x@Nothing y@Nothing -> 0
