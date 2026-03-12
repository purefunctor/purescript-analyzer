-- | This shadows the test prelude to remove the representation types.
module Data.Generic.Rep where

class Generic a rep | a -> rep where
  to :: rep -> a
  from :: a -> rep
