module Main where

foreign import data Proxy :: forall k. k -> Type

type Simple = Proxy "hello"

type WithEscape = Proxy "hello \" world"

type WithBackslash = Proxy "hello \\ world"

type WithNewline = Proxy "hello \n world"

type RawEmpty = Proxy """"""

type RawSimple = Proxy """hello world"""

type RawWithQuote = Proxy """"hello""""
