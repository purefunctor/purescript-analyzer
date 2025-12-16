module BinderConstructorNestedQualified where

test a = case a of
  M.Tuple M.Nothing M.Nothing -> 0
  M.Just (N.Just x) -> x
