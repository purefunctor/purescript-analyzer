module Prim.Symbol where

import Prim.Ordering (Ordering)

class Append :: Symbol -> Symbol -> Symbol -> Constraint
class Append left right appended | left right -> appended, right appended -> left, appended left -> right

class Compare :: Symbol -> Symbol -> Ordering -> Constraint
class Compare left right ordering | left right -> ordering

class Cons :: Symbol -> Symbol -> Symbol -> Constraint
class Cons head tail symbol | head tail -> symbol, symbol -> head tail
