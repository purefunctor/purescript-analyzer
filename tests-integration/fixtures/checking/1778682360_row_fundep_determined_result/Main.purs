module Main where

import Prim.Row as Row
import Prim.RowList as RL

data Maybe a = Nothing | Just a

data Nullable a

class ExtractType entry typ | entry -> typ
instance ExtractType String String
instance ExtractType (Nullable a) (Maybe a)

class StripColumnsRL :: RL.RowList Type -> RL.RowList Type -> Constraint
class StripColumnsRL rl out | rl -> out

instance StripColumnsRL RL.Nil RL.Nil
instance (ExtractType entry typ, StripColumnsRL tail out') => StripColumnsRL (RL.Cons name entry tail) (RL.Cons name typ out')

class ListToRow :: RL.RowList Type -> Row Type -> Constraint
class ListToRow list row | list -> row

instance ListToRow RL.Nil ()
instance (Row.Cons name typ rowTail row, ListToRow tail rowTail) => ListToRow (RL.Cons name typ tail) row

class StripColumns :: Row Type -> Row Type -> Constraint
class StripColumns columns row | columns -> row

instance (RL.RowToList columns list, StripColumnsRL list outList, ListToRow outList row) => StripColumns columns row

type Columns =
  ( account_guid :: Nullable String
  )

test :: forall row. StripColumns Columns row => Record row
test = { account_guid: Nothing }

bad :: forall row. StripColumns Columns row => Record row
bad = { other: "acct" }
