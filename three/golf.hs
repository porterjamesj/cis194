module Golf where

import Data.List
import Data.List.Split

-- TODO annoyed that this uses last
--
-- it's safe here because the result of chunksOf never contains an
-- empty list, but still
skipsN :: [a] -> Int -> [a]
skipsN xs n = map last (dropWhileEnd (\x -> length x < n) split)
  where split = chunksOf n xs

skips :: [a] -> [[a]]
skips xs = map (skipsN xs) [1..(length xs)]
