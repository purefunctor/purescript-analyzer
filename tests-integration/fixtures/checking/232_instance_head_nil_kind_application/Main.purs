module Main where

import Prim.RowList as RL

class ListToRow :: RL.RowList Type -> Constraint
class ListToRow xs

instance listToRowNil :: ListToRow RL.Nil

class ListToRow2 :: RL.RowList Type -> RL.RowList Type -> Constraint
class ListToRow2 xs ys

instance listToRow2Nil :: ListToRow2 RL.Nil RL.Nil
