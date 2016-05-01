{-# LANGUAGE FlexibleInstances #-}

import Data.Monoid

import Scrabble
import Sized
import Buffer
import Editor

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)


tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m


(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) a b = Append (tag a `mappend` tag b) a b


single :: a -> JoinList Size a
single a = Single (Size 1) a

-- These are all probably expressable as folds somehow, I am too tired
-- to figure it out though.


indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ i (Single m a)
  | i == 0 = Just a
  | otherwise = Nothing
indexJ i jl@(Append s left right)
  | i < 0 = Nothing
  | i > thisSize = Nothing
  | i < leftSize = indexJ i left
  | otherwise = indexJ (i - leftSize) right
  where
    thisSize = getSize $ size s
    leftSize = getSize $ size $ tag left


dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty = Empty
dropJ i jl@(Single _ _)
  | i <= 0 = jl
  | otherwise = Empty
dropJ i jl@(Append s left right)
  | i <= 0 = jl
  | i < leftSize = (dropJ i left) +++ right
  | i == leftSize = right
  | otherwise = dropJ (i-leftSize) right
  where
    thisSize = getSize $ size s
    leftSize = getSize $ size $ tag left


takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty = Empty
takeJ i jl@(Single _ _)
  | i >= 1 = jl
  | otherwise = Empty
takeJ i jl@(Append s left right)
  | i <= 0 = Empty
  | i < leftSize = takeJ i left
  | i == leftSize = left
  | otherwise = left +++ (takeJ (i-leftSize) right)
  where
    thisSize = getSize $ size s
    leftSize = getSize $ size $ tag left


scoreLine :: String -> JoinList Score String
scoreLine s = (Single (scoreString s) s)


instance Buffer (JoinList (Score, Size) String) where
  toString Empty = ""
  toString (Single _ a) = a
  toString (Append _ left right) = toString left ++ "\n" ++ toString right

  fromString "" = Empty
  fromString a = foldl (+++) Empty singles
    where
      singles = zipWith Single ms ls
      ms = zip (map scoreString ls) (repeat (Size 1))
      ls = lines a

  line = indexJ

  replaceLine i s jl = (takeJ i jl) +++ (fromString s) +++ (dropJ (i+1) jl)

  numLines = getSize . snd . tag

  value = getScore . fst. tag


main :: IO ()
main = runEditor editor $ (fromString $ unlines
         [ "This buffer is for notes you don't want to save, and for"
         , "evaluation of steam valve coefficients."
         , "To load a different file, type the character L followed"
         , "by the name of the file."
         ] :: (JoinList (Score, Size) String))
