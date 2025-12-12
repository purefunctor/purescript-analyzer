module Main where

-- Open row with row polymorphism
type HasName r = { name :: String | r }

-- Open row type alias
type HasNameRow r = ( name :: String | r )
