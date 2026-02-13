module Main where

import Lib as L

test = L.do
  life <- L.pure 42
  L.pure life
