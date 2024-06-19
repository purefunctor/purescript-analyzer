module Main where

import Lib
import Lib as Qualified
import Lib (Type, Data(..), List(Cons, List), value, class Class)
import Lib (Type, Data(..), List(Cons, List), value, class Class) as Qualified
import Lib hiding (Type, Data(..), List(Cons, List), value, class Class)
import Lib hiding (Type, Data(..), List(Cons, List), value, class Class) as Qualified
