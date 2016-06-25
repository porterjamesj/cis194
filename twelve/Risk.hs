{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad
import Control.Monad.Loops -- cabal install monad-loops
import Control.Monad.Random
import System.Random
import Data.List

newtype DieValue = DV { unDV :: Int }
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom



type Army = Int

data Outcome = AttackerWin
             | DefenderWin
  deriving Eq

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }
  deriving Show

sortDesc = sortBy (flip compare)

battle :: Battlefield -> Rand StdGen Battlefield
battle bf@(Battlefield attackers defenders) = do
  attackerDice <- replicateM (min (attackers-1) 3) die
  defenderDice <- replicateM (min defenders 2) die
  return $ foldr applyOutcome bf (toOutcomes attackerDice defenderDice)

toOutcomes :: [DieValue] -> [DieValue] -> [Outcome]
toOutcomes attackerDice defenderDice =
  zipWith determineOutcome (sortDesc attackerDice) (sortDesc defenderDice)

-- TODO think about how to avoid going below zero attackers or
-- defenders
applyOutcome :: Outcome -> Battlefield -> Battlefield
applyOutcome o (Battlefield attackers defenders)
  | o == AttackerWin = Battlefield attackers (defenders-1)
  | otherwise = Battlefield (attackers-1) defenders

determineOutcome :: DieValue -> DieValue -> Outcome
determineOutcome attacker defender
  | unDV attacker > unDV defender = AttackerWin
  | otherwise = DefenderWin


battleDone :: Battlefield -> Bool
battleDone (Battlefield as ds) = as < 2 || ds == 0

invade :: Battlefield -> Rand StdGen Battlefield
invade bf = iterateUntilM battleDone battle bf

successProb :: Battlefield -> Rand StdGen Double
successProb bf = do
  results <- replicateM 1000 (invade bf)
  let defeats = (length . (filter ((>0) . defenders))) results
  return $ fromIntegral defeats / 1000
