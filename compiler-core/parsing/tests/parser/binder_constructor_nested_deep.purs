module BinderConstructorNestedDeep where

test a = case a of
  Just (Just (Just x)) -> x
  Just (Just Nothing) -> 0
  Cons x (Cons y (Cons z Nil)) -> x
