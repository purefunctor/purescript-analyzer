module Control.Monad where

import Control.Applicative (class Applicative)
import Control.Bind (class Bind)

class (Applicative m, Bind m) <= Monad m
