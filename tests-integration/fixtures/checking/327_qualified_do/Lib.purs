module Lib where

foreign import bind :: forall m a b. m a -> (a -> m b) -> m b
foreign import discard :: forall m a b. m a -> (a -> m b) -> m b
foreign import pure :: forall m a. a -> m a
