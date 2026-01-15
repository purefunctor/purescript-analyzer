module Main where

import Safe.Coerce (coerce)
import Lib (Maybe(..), MaybeAlias(..), Container)

coerceContainer :: Container Maybe -> Container MaybeAlias
coerceContainer = coerce

coerceContainerReverse :: Container MaybeAlias -> Container Maybe
coerceContainerReverse = coerce
