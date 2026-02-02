module Main where

foo :: Int -> Int
foo x | a <- 123 = a

bar :: String -> String
bar s | b <- "hello" = b

foo' x | c <- 42 = c

bar' s | b <- "hello" = b
