module RecordItemLabels where

invalid { "error" } = { "error" }
invalid { forall } = { forall }
invalid { true, false } = { true, false }
