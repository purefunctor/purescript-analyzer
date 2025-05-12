module ImportUnqualifiedHiding (module Library) where

import Library hiding (hideMe, HideTy(..), MultiTy(Ctor2))
