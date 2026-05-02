module Class where

class Marker m where
  marker :: m

class Lookup a b | a -> b where
  lookup :: a -> b
