module Prim.TypeError where

data Doc :: Type
data Doc

class Warn :: Doc -> Constraint
class Warn message

class Fail :: Doc -> Constraint
class Fail message

data Text :: Symbol -> Doc
data Text

data Quote :: forall (k :: Type). k -> Doc
data Quote t

data QuoteLabel :: Symbol -> Doc
data QuoteLabel n

data Beside :: Doc -> Doc -> Doc
data Beside l r

data Above :: Doc -> Doc -> Doc
data Above u d
