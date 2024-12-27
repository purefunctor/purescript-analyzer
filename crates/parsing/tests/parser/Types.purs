module Type where

kinded :: T :: T

arrow :: A -> B
arrow :: A -> B -> C

constrained :: A => B
constrained :: A => B => C

operator :: A + B + C

application :: F a b

string :: "hello"
string :: """hello"""

integer :: 123
negated :: -1

operator_name :: (+)

hole :: ?hole

wildcard :: _

prefixed :: Data.Tuple.Tuple
prefixed :: Data.Tuple.Nested.(/)
