module Lecture3 where

import Data.Char ( toUpper )

data Thing = Shoe
           | Ship
           | SealingWax
           | Cabbage
           | King
             deriving Show

shoe :: Thing
shoe = Shoe

listO'Things :: [Thing]
listO'Things = [Shoe, SealingWax, King, Cabbage, King]

isSmall :: Thing -> Bool
isSmall Shoe = True
isSmall Ship = False
isSmall SealingWax = True
isSmall Cabbage = True
isSmall King = False

isSmall2 :: Thing -> Bool
isSmall2 Ship = False
isSmall2 King = False
isSmall2 _    = True

data FailableDouble = Failure
                    | OK Double
                      deriving Show

ex01 :: FailableDouble
ex01 = Failure
ex02 :: FailableDouble
ex02 = OK 3.4

safeDiv :: Double -> Double -> FailableDouble
safeDiv _ 0 = Failure
safeDiv x y = OK (x / y)

failuretoZero :: FailableDouble -> Double
failuretoZero Failure = 0
failuretoZero (OK d) = d

data Person = Person String Int Thing
              deriving Show

richard :: Person
richard = Person "Richard" 32 SealingWax

stan :: Person
stan = Person "Stan" 94 Cabbage

getAge :: Person -> Int
getAge (Person _ a _) = a

baz :: Person -> String
baz p@(Person n _ _) = "The name field of (" ++ show p ++ ") is " ++ n

checkFav :: Person -> String
checkFav (Person n _ SealingWax) = n ++ ", you're my kind of person!"
checkFav (Person n _ _) = n ++ ", your fav thing is lame."

ex03 :: Int
ex03 = case "Hello" of
         [] -> 3
         ('H':s) -> length s
         _ -> 7
