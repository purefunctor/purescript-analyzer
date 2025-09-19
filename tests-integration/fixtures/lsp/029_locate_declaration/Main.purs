module Main where

-- | A safe abstraction over side-effects
foreign import data Effect :: Type -> Type
--                  $ @ %

type EffectInt = Effect Int
