module Prim.Row where

class Union :: forall (k :: Type). Row k -> Row k -> Row k -> Constraint
class Union left right union | left right -> union, right union -> left, union left -> right

class Nub :: forall (k :: Type). Row k -> Row k -> Constraint
class Nub original nubbed | original -> nubbed

class Lacks :: forall (k :: Type). Symbol -> Row k -> Constraint
class Lacks label row

class Cons :: forall (k :: Type). Symbol -> k -> Row k -> Row k -> Constraint
class Cons label a tail row | label a tail -> row, label row -> a tail
