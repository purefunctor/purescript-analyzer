module Main where

data SList

foreign import data SCons :: Symbol -> SList -> SList
foreign import data SNil :: SList
