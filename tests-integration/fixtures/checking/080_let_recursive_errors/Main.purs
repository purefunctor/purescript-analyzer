module Main where

foreign import id :: forall a. a -> a

-- Type mismatch in mutual recursion (signatures conflict)
typeMismatch :: Int -> Int
typeMismatch n =
  let f :: Int -> Int
      f x = g x
      g :: Int -> String
      g x = f x
  in f n

-- Occur check failure (infinite type: f :: a -> a -> a -> ...)
occurCheck :: Int -> Int
occurCheck n =
  let f x = f
  in f n

-- Mutual recursion with conflicting inferred types
inferConflict :: Int -> Int
inferConflict n =
  let f x = g x
      g x = f "hello"
  in f n

-- Three-way with type conflict
threeWayConflict :: Int -> Int
threeWayConflict n =
  let a :: Int -> Int
      a x = b x
      b x = c "oops"
      c x = a x
  in a n
