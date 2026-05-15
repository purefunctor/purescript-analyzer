module Main where

import Module1
import Module2 (JSX, div)

test :: JSX -> JSX
test = div { className: "header" }
