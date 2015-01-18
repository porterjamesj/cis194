module Golf where

import Data.List
import Data.List.Split

skipsN :: [a] -> Int -> [a]
skipsN xs n = map last (dropWhileEnd (\x -> length x < n) split)
  where split = chunksOf n xs

skips :: [a] -> [[a]]
skips xs = map (skipsN xs) [1..(length xs)]
