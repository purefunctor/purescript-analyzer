module Main where

import Control.Bind (discard)
import Data.Unit (Unit)
import Effect (Effect)

foreign import delay :: Int -> Effect Unit

test :: Effect Unit
test = alias
  where
  alias = implementation
  sleep = delay
  implementation = do
    sleep timeout
    delay timeout
    where
    timeout = 42
