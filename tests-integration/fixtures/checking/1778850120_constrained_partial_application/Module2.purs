module Module2 where

foreign import data JSX :: Type

foreign import div :: { className :: String } -> JSX -> JSX
