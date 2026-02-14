module Main where

foreign import unsafeCoerce :: forall a b. a -> b

data Query input a = Query input a
data HM state m a = HM state (m a)
data Solid input m

type Spec state input m =
  { eval :: forall a. Query input a -> HM state m a
  }

mkSpec
  :: forall state input m
   . Spec state input m
  -> Solid input m
mkSpec = unsafeCoerce

unSpec
  :: forall input m a
   . (forall state. Spec state input m -> a)
  -> Solid input m
  -> a
unSpec = unsafeCoerce

hoist
  :: forall input m
   . Solid input m
  -> Solid input m
hoist = unSpec \c -> mkSpec
  { eval: c.eval
  }
