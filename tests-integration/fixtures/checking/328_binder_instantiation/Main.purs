module Main where

data Maybe a = Just a | Nothing
data Id = MkId (forall a. a -> a)

identity :: forall a. Maybe (a -> a)
identity = Nothing

test :: Partial => Int
test = case identity of
  Just f -> let _ = f 42 in f true

test2 :: Id -> Boolean
test2 x = case x of
  MkId f -> let _ = f 42 in f true

test3 :: Partial => Int
test3 =
  let (Just f) = identity
  in let _ = f 42 in f true

test4 :: Id -> Boolean
test4 x =
  let (MkId f) = x
  in let _ = f 42 in f true
