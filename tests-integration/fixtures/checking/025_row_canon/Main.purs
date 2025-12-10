module Main where

-- Tests canonicalization: fields sorted by label, duplicates preserve order
-- { b :: String, a :: Int } canonicalizes to ( a :: Int, b :: String )
type UnorderedRow = ( b :: String, a :: Int )

-- With duplicates: { b :: String, a :: Int, a :: String }
-- canonicalizes to ( a :: Int, a :: String, b :: String )
type DuplicateUnordered = ( b :: String, a :: Int, a :: String )
