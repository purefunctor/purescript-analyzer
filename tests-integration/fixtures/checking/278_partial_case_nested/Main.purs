module Main where

import Partial.Unsafe (unsafePartial)

partialCase :: Partial => Int
partialCase = case 123 of
  123 -> 123

partialCase' = case 123 of
  123 -> 123

partialNested :: Int
partialNested = unsafePartial (case 123 of
  123 -> 123)

partialNested' = unsafePartial (case 123 of
  123 -> 123)
