module ImportQualifiedHiding (module L) where

import Library hiding (hideMe, HideTy(..), MultiTy(Ctor2)) as L
