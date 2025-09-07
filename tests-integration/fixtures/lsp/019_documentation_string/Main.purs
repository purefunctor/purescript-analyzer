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

-- | Documentation only
docOnly = 123

test3 = docOnly
--      $

-- | Line 1
-- | Line 2
-- | Line 3
multiLine = 123

test4 = multiLine
--      $

-- | Line 1
--
-- | Line 2
--
-- | Line 3
multiLine2 = 123

test5 = multiLine2
--      $


-- | Line 1
-- |
-- | Line 2
-- |
-- | Line 3
multiLine3 = 123

test6 = multiLine3
--      $

letBinding =
  let
    valueOnly = 123

    withSignature :: Int
    withSignature = 123
  in
    [ valueOnly
--    $
    , withSignature
--    $
    ]
