{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Scrabble where

import Data.Monoid
import Data.Char (toLower)
import Data.Map (fromList, findWithDefault, Map)

-- should probably be newtype but im lazy
newtype Score = Score { getScore :: Int }
  deriving (Show, Eq, Ord, Num, Real, Integral, Enum)

instance Monoid Score where
  mempty = 0
  mappend = (+)

alphabet :: String
alphabet = "abcdefghijklmnopqrstuvwxyz"

scrabbleScores :: [Score]
scrabbleScores = [1,3,3,2,1,4,2,4,1,8,5,1,3,1,1,3,10,1,1,1,1,4,4,8,4,10]

scoreMap :: Map Char Score
scoreMap = fromList $ zip alphabet scrabbleScores

score :: Char -> Score
score c = findWithDefault 0 (toLower c) scoreMap

scoreString :: String -> Score
scoreString = sum . map score
