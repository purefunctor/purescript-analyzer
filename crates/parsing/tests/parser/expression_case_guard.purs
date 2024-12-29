module ExpressionCaseGuard where

main = case n of
  0 | n == 0 -> true
  _ | m <- n
    , m == n -> false
