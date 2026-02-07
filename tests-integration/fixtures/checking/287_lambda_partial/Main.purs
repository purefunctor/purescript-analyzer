module Main where

import Partial.Unsafe (unsafePartial)

data Maybe a = Just a | Nothing

isJust = \(Just _) -> true
isNothing = \Nothing -> true

unsafeIsJust = unsafePartial isJust
unsafeIsNothing = unsafePartial isNothing
