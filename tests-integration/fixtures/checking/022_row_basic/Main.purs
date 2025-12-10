module Main where

-- Basic record type with multiple fields
type Person = { name :: String, age :: Int }

-- Row type alias
type PersonRow = ( name :: String, age :: Int )

-- Record type using an explicit row
type PersonRecord = Record ( name :: String, age :: Int )
