module Main where

data Maybe a = Just a | Nothing

complete :: Maybe Int -> Maybe Int -> Int
complete (Just _) (Just _) = 1
complete (Just _) Nothing = 2
complete Nothing (Just _) = 3
complete Nothing Nothing = 4

incomplete1 :: Maybe Int -> Maybe Int -> Int
incomplete1 (Just _) Nothing = 2
incomplete1 Nothing (Just _) = 3
incomplete1 Nothing Nothing = 4

incomplete2 :: Maybe Int -> Maybe Int -> Int
incomplete2 (Just _) (Just _) = 1
incomplete2 Nothing (Just _) = 3
incomplete2 Nothing Nothing = 4

incomplete3 :: Maybe Int -> Maybe Int -> Int
incomplete3 (Just _) (Just _) = 1
incomplete3 (Just _) Nothing = 2
incomplete3 Nothing Nothing = 4

incomplete4 :: Maybe Int -> Maybe Int -> Int
incomplete4 (Just _) (Just _) = 1
incomplete4 (Just _) Nothing = 2
incomplete4 Nothing (Just _) = 3
