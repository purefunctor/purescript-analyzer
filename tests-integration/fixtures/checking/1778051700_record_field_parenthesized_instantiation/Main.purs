module Main where

const :: forall @a @b. a -> b -> a
const a _ = a

choose :: Int -> (forall @a @b. a -> b -> a)
choose _ a _ = a

normal =
  { parenthesised: (const)
  }

prenex =
  { variable: choose
  , application: choose 0
  }
