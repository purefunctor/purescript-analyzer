module Main where

data Unit = Unit

unit = case _ of
  Unit -> 1
  _ -> 2
  Unit -> 3

data YesNo = Yes | No

yes = case _ of
  Yes -> 1
  _ -> 2
  Yes -> 3

no = case _ of
  Yes -> 1
  _ -> 2
  No -> 3

yesNo = case _ of
  Yes -> 1
  No -> 2
  _ -> 3
