module Main where

foreign import data Proxy :: forall k. k -> Type

type Positive = Proxy 123

type Negative = Proxy (-1)

type Underscore = Proxy 123_456

type LeadingZero = Proxy 00_123

type Hex = Proxy 0xFFFFFF
