module Main where

bareTail :: forall opts. ( | opts ) -> Int
bareTail _ = 1

plainTail :: forall opts. opts -> Int
plainTail _ = 2

recordTail :: forall opts. { | opts } -> Int
recordTail _ = 3

withBareTail = bareTail { option: 1 }
withPlainTail = plainTail { option: 1 }
withRecordTail = recordTail { option: 1 }
