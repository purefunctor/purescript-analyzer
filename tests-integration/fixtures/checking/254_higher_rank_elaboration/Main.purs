module Main where

class IsSymbol (sym :: Symbol) where
  reflectSymbol :: Proxy sym -> String

data Proxy (a :: Symbol) = Proxy

reifySymbol :: forall r. String -> (forall sym. IsSymbol sym => Proxy sym -> r) -> r
reifySymbol s f = coerce f { reflectSymbol: \_ -> s } Proxy
  where
  coerce
    :: (forall sym1. IsSymbol sym1 => Proxy sym1 -> r)
    -> { reflectSymbol :: Proxy "" -> String }
    -> Proxy ""
    -> r
  coerce = unsafeCoerce

foreign import unsafeCoerce :: forall a b. a -> b

test :: String -> String
test s = reifySymbol s reflectSymbol
