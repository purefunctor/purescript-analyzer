module TypeRecord where

empty :: {}

tail :: { | r }

item :: { a :: Type }

itemTail :: { a :: Type | r }

items :: { a :: Type, b :: Type }

itemsTail :: { a :: Type, b :: Type | r }

