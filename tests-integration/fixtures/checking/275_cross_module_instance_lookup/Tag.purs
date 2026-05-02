module Tag where

import Class (class Marker, class Lookup)

newtype Tag = Tag Int

newtype Member = Member Int

instance Marker Member where
  marker = Member 0

instance Lookup Tag Member where
  lookup _ = Member 0
