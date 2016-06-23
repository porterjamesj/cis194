{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad
import Control.Monad.Random
import System.Random

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

battle :: Battlefield -> Rand StdGen Battlefield
battle bf@(Battlefield attackers defenders) = do
  attackerDice <- replicateM (max (attackers-1) 3) die
  defenderDice <- replicateM (max defenders 2) die
  return $ foldr applyOutcome bf (zipWith determineOutcome attackerDice defenderDice)


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
