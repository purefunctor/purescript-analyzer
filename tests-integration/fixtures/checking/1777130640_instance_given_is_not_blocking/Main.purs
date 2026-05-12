module Main where

foreign import unsafeSet :: forall r1 r2 a. String -> a -> Record r1 -> Record r2

class BuildRecord :: Row Type -> Row Type -> Constraint
class BuildRecord row subrow | row -> subrow where
  buildIt :: Record subrow

instance buildRecordImpl :: BuildRecord row subrow where
  buildIt = result
    where
    result :: Record subrow
    result = unsafeSet "x" 42 {}

test :: forall r s. BuildRecord r s => Record s
test = buildIt
