module Main where

-- Empty closed row
type EmptyRow = ()

-- Empty record
type EmptyRecord = {}

-- Tail-only row (open row with no fields)
type TailOnly r = ( | r )

-- Tail-only record
type TailOnlyRecord r = { | r }
