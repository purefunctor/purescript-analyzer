module Main where

class Step f a x a' | f a x -> a' where
  step :: f -> a -> x -> a'

class Done f a x where
  done :: f -> a -> x

class Walk f a r where
  walk :: f -> a -> r

instance walkArg ::
  ( Step f a x a'
  , Walk f a' xs
  ) =>
  Walk f a (x -> xs) where
  walk f a x = walk f (step f a x)

else instance walkBase ::
  ( Done f a x
  ) =>
  Walk f a x where
  walk = done
