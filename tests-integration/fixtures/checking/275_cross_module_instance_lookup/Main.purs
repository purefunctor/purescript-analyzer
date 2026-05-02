module Main where

import Class (class Marker, class Lookup)
import Tag (Tag(..))

useBoth :: forall s m. Marker m => Lookup s m => s -> Int
useBoth _ = 0

test :: Int
test = useBoth (Tag 0)
