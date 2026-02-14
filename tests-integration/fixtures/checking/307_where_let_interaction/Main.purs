module Main where

class MyClass a where
  method :: a -> Int

instance MyClass Int where
  method _ = 42

test :: forall a. MyClass a => a -> Int
test _ =
  let go x = method x
  in go 42

test2 :: forall a. MyClass a => a -> Int
test2 _ =
  let go :: _ -> _
      go x = method x
  in go 42
