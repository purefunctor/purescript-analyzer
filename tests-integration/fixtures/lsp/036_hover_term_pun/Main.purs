module Main where

inferred { argument } = { argument }
--         $              $

checked :: forall ofCourse. { argument :: ofCourse } -> { argument :: ofCourse }
checked { argument } = { argument }
--        $              $
