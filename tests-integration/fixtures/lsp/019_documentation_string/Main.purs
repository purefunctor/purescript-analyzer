module Main where

-- | The identity function
id :: forall a. a -> a
id a = a

test = id
--     $

const :: forall a b.
  a ->
  b ->
  a
const a _ = a

test2 = const
--      $
