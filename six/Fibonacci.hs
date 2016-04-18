{-# LANGUAGE FlexibleInstances #-}

import Data.List

-- fib

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = map fib [0..]


fibs2 :: [Integer]
fibs2 = 0 : 1 : map addPair fibPairs
  where
    addPair = uncurry (+)
    fibPairs = zip fibs2 $ tail fibs2


-- streams

data Stream a = Cons a (Stream a)

instance Show a => Show (Stream a) where
  show (Cons fst (Cons snd (Cons thd _))) =
    "[" ++ (concat $ intersperse " " $ reprs) ++ " ... ]"
    where reprs = map show [fst, snd, thd]

-- simple streams

onesStream = Cons 1 onesStream

letterAStream = Cons "a" letterAStream

-- stream functions

streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x : streamToList xs

streamRepeat :: a -> Stream a
streamRepeat x = Cons x $ streamRepeat x

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x xs) = Cons (f x) (streamMap f xs)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Cons x (streamFromSeed f $ f x)

-- moar stream definitions

nats :: Stream Integer
nats = streamFromSeed (+1) 0

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons x xs) other =
  Cons x $ interleaveStreams other xs

-- there is probably a simpler way to do this without the function

makeRuler :: Integer -> Stream Integer
makeRuler n = interleaveStreams (streamRepeat n) (makeRuler $ n+1)

ruler = makeRuler 0
