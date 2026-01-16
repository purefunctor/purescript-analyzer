module Main where

import Data.Newtype (class Newtype, wrap, unwrap)

newtype UserId = UserId Int

derive instance Newtype UserId _

wrapUserId :: Int -> UserId
wrapUserId = wrap

unwrapUserId :: UserId -> Int
unwrapUserId = unwrap

newtype Wrapper a = Wrapper a

derive instance Newtype (Wrapper a) _

wrapWrapper :: forall a. a -> Wrapper a
wrapWrapper = wrap

unwrapWrapper :: forall a. Wrapper a -> a
unwrapWrapper = unwrap
