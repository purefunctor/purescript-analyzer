module Main where

fromBinder argument = argument
--         $

fromLiteral "hello" = "hello"
--          $

fromArray [ element ] = [ element ]
--        $

fromRecord { element } = { element }
--         $
