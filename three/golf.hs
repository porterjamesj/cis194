

module Golf where

import Data.List
import Data.List.Split
import Data.Maybe

-- skips

-- TODO annoyed that this uses last
--
-- it's safe here because the result of chunksOf never contains an
-- empty list, but still
skipsN :: [a] -> Int -> [a]
skipsN xs n = map last (dropWhileEnd (\x -> length x < n) split)
  where split = chunksOf n xs

skips :: [a] -> [[a]]
skips xs = map (skipsN xs) [1..(length xs)]

-- localMaxima
-- I am dissatisfied that windows is recursive, but otherwise pretty happy with this

windows3 :: [a] -> [(a,a,a)]
windows3 (x:x':x'':xs) = (x,x',x'') : windows3 (x':x'':xs)
windows3 _ = []

maxMiddle :: (Integer, Integer, Integer) -> Maybe Integer
maxMiddle (a,b,c)
  | b > a && b > c = Just b
  | otherwise = Nothing

localMaxima :: [Integer] -> [Integer]
localMaxima xs = catMaybes $ map maxMiddle (windows3 xs)

histogram :: [Integer] -> String
