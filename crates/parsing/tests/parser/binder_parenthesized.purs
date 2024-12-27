module BinderParenthesized where

parenthesized (x) = 0
parenthesized (x + y) = 0
parenthesized (((x))) = 0
