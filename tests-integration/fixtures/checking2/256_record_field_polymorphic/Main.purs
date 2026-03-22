module Main where

data Either a b = Left a | Right b

newtype FFIUtil = FFIUtil
  { left :: forall a b. a -> Either a b
  , right :: forall a b. b -> Either a b
  , isLeft :: forall a b. Either a b -> Boolean
  , isRight :: forall a b. Either a b -> Boolean
  }

isLeft :: forall a b. Either a b -> Boolean
isLeft (Left _) = true
isLeft (Right _) = false

isRight :: forall a b. Either a b -> Boolean
isRight (Left _) = false
isRight (Right _) = true

test :: FFIUtil
test = FFIUtil
  { left: Left
  , right: Right
  , isLeft
  , isRight
  }
