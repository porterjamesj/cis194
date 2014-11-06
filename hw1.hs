import Data.Char
import Control.Exception.Base -- for `assert`
import Data.List

toDigits :: Integer -> [Integer]
toDigits n
  | n <= 0 = []
  | otherwise = [(toInteger . digitToInt) c | c <- show n]

toDigitsRev :: Integer -> [Integer]
toDigitsRev n = reverse (toDigits n)

-- doubleEveryOther :: [Integer] -> [Integer]
-- doubleEveryOther xs = reverse (zipWith (*) (cycle [1,2]) (reverse xs))
doubleEveryOther xs = snd . mapAccumR (\a x -> (not a, (if a then 2*x else x))) False


main :: IO ()
main = do
  putStrLn (assert (toDigits 1234 == [1,2,3,4]) "success")
  putStrLn (assert (toDigitsRev 1234 == [4,3,2,1]) "success")
  putStrLn (assert (toDigits 0 == []) "success")
  putStrLn (assert (toDigits (-17) == []) "success")
  -- putStrLn (assert  (doubleEveryOther [8,7,6,5] == [16,7,12,5]) "success")
  -- putStrLn (assert  (doubleEveryOther [1,2,3] == [1,4,3]) "success")
  -- putStrLn (assert (sumDigits [16,7,12,5]) == 22)
