module Main where

import Prim.Row as Row

foreign import unsafeCoerce :: forall a b. a -> b

type Required = (key :: String)

foreign import consume
  :: forall given given_
   . Row.Union given given_ Required
  => Record given
  -> String

merge
  :: forall r1 r2 r3 r4
   . Row.Union r1 r2 r3
  => Row.Nub r3 r4
  => Record r1
  -> Record r2
  -> Record r4
merge _ _ = unsafeCoerce {}

class Collect a where
  collect :: a -> Array String

instance Collect (Array String) where
  collect xs = xs

combine
  :: forall partial duplicated deduped deduped_ items
   . Collect items
  => Row.Union (key :: String) partial duplicated
  => Row.Nub (key :: String | partial) deduped
  => Row.Union deduped deduped_ Required
  => Record partial
  -> items
  -> String
combine provided _ = consume merged
  where
  merged :: Record deduped
  merged = merge { key: "value" } provided

combine' :: forall items. Collect items => items -> String
combine' = combine {}

repro :: String
repro = combine {} ([] :: Array String)
