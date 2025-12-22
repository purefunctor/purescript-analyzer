module Main where

foreign import data Void :: Type
foreign import absurd :: forall a. Void -> a

-- Self-recursive let binding (should work)
selfRecursive :: Void -> Int
selfRecursive v =
  let f x = f x
  in f v

-- Mutually recursive let bindings (works with SCC tracking)
mutualRecursive :: Void -> Int
mutualRecursive v =
  let f x = g x
      g x = f x
  in f v

-- Three-way mutual recursion
threeWay :: Void -> Int
threeWay v =
  let a x = b x
      b x = c x
      c x = a x
  in a v

-- Pattern boundary test: f/g mutual, h/i mutual (separate chunks)
patternBoundary :: Void -> Int
patternBoundary v =
  let f x = g x
      g x = f x
      _ = f v
      h x = i x
      i x = h x
  in h v

-- Pure checking: all have signatures
pureChecking :: Int -> Int
pureChecking n =
  let f :: Int -> Int
      f x = g x
      g :: Int -> Int
      g x = f x
  in f n

-- Mixed: some have signatures, some don't
mixedRecursion :: Int -> Int
mixedRecursion n =
  let f :: Int -> Int
      f x = g x
      g x = f x
  in f n
