module Main where

foreign import data Effect ∷ Type → Type

foreign import data Either ∷ Type → Type → Type

bug ∷ ∀ a. Effect (Either a a) → Effect (Either a a)
--                 $                     $
bug effect = effect
