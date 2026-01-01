module Main where

import Prim.Boolean (True, False)

data Proxy a = Proxy

-- A class where the result depends on comparing two types
class IsZero n r | n -> r

foreign import data Z :: Type  -- Zero
foreign import data S :: Type -> Type  -- Successor

instance isZeroZ :: IsZero Z True
instance isZeroS :: IsZero (S n) False

-- Another class that uses the result
class And a b r | a b -> r

instance andTT :: And True True True
instance andTF :: And True False False
instance andFT :: And False True False
instance andFF :: And False False False

-- Test: if IsZero derives r1 ~ True and r2 ~ True,
-- then And r1 r2 should resolve to True
test :: forall r1 r2 r. IsZero Z r1 => IsZero Z r2 => And r1 r2 r => Proxy r
test = Proxy

forceSolve = { test }
