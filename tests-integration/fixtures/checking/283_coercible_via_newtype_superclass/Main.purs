module Main where

import Data.Newtype (class Newtype, unwrap)
import Lib (HiddenInt)
import Safe.Coerce (coerce)

useUnwrap :: forall t. Newtype t HiddenInt => t -> HiddenInt
useUnwrap = unwrap

useCoerce :: forall t. Newtype t HiddenInt => t -> HiddenInt
useCoerce = coerce
