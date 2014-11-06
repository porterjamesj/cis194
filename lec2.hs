module Lecture2 where

strLength :: String -> Int
strLength [] = 0
strLength (_:xs) = let len_rest = strLength xs in
                   len_rest + 1

frob :: String -> Char
frob [] = 'a'
frob str
     | len > 5 = 'x'
     | len < 3 = 'y'
     | otherwise = 'z'
     where
       len = strLength str

sumTo20 :: [Int] -> Int
sumTo20 nums = sumTo20Helper 0 nums  -- acc starts at zero

sumTo20Helper :: Int -> [Int] -> Int
sumTo20Helper acc [] = acc  -- base case, return accumulator
sumTo20Helper acc (x:xs)
    | acc >= 20 = acc
    | otherwise = sumTo20Helper (acc + x) xs

bogus :: Int -> String
bogus x
    | x > 5 = "big"
    | x < 0 = "small"
