module Data.Symbol where

import Type.Proxy (Proxy)

class IsSymbol (sym :: Symbol) where
  reflectSymbol :: Proxy sym -> String
