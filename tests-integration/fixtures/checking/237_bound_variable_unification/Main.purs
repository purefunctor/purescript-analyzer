module Main where

-- Test based on Data.Bounded's boundedRecordCons pattern where
-- a bound row variable appears in a type annotation within a where clause.

foreign import unsafeSet :: forall r1 r2 a. String -> a -> Record r1 -> Record r2

class BuildRecord :: Row Type -> Row Type -> Constraint
class BuildRecord row subrow | row -> subrow where
  buildIt :: Record subrow

instance buildRecordImpl :: BuildRecord row subrow where
  buildIt = result
    where
    -- Type annotation references the bound variable `subrow`
    result :: Record subrow
    result = unsafeSet "x" 42 {}

test :: forall r s. BuildRecord r s => Record s
test = buildIt
