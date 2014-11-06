module HW02 where

import Words
import Data.List

-- Though a Scrabble hand is the same Haskell type as a Scrabble word, they
-- have different properties. Specifically, a hand is unordered whereas a word
-- is ordered. We denote this distinction by using a type synonym to talk
-- about hands, even though we could just say `String`.
type Hand = [Char]

-- A `Template` is like a word, but it has '?' characters in some places as
-- placeholders for letters from a player's hand. Because real words do not
-- have '?' characters, we use another type synonym to track this distinction.
type Template = String

-- A 'STemplate' is like a template, but it has markers to indicate four kinds
-- of special board locations: double-letter noted with 'D', triple-letter
-- noted with 'T', double-word noted with '2', and triple-word noted with '3'.
-- For matching, these behave just like '?' does -- they can be filled in with
-- any letter. But, when scoring, any letter played on a 'D' gets double its
-- value, and any letter played on a 'T' gets triple its value. If any square
-- in the template is a '2', the whole word's value is doubled; if any square
-- in the template is a '3', the whole word's score is tripled. If multiple of
-- these special squares are in the same word, the effects multiply.
type STemplate = Template

-- Write your code below:
formableBy :: String -> Hand -> Bool
formableBy [] _ = True -- empty string can be formed by any hand
formableBy (l:ls) hand
    | l `elem` hand = formableBy ls (delete l hand)
    | otherwise = False

-- formableBy "fun" ['x', 'n', 'i', 'f', 'e', 'u', 'l']
-- formableBy "haskell" [''k',''l',''e',''h',''a',''l',''s'] == True
-- formableBy "haskell" ['k','l','e','h','a','l','s'] == True
-- formableBy "haskell" ['k','l','e','h','a','y','s'] == True


wordsFrom :: Hand -> [String]
wordsFrom hand = filter (formableBy hand) allWords

-- wordsFrom ['h','e','l','l','o'] == [ "eh","el","ell","he","hell","hello","helo" , "ho","hoe","hole","lo","oe","oh","ole" ]

wordFitsTemplate :: Template -> Hand -> String -> Bool
wordFitsTemplate [] _ [] = True
wordFitsTemplate _ _ [] = False -- if there's leftover space in the
                                -- template, it doesn't match.
wordFitsTemplate [] _ _ = False -- if we run out space in the
                                -- template, we're screwed.
wordFitsTemplate (t:ts) hand (w:ws)
    | t == w = wordFitsTemplate ts hand ws
    | t == '?' && w `elem` hand = wordFitsTemplate ts (delete w hand) ws
    | otherwise = False

-- wordFitsTemplate "??r?" ['c','x','e','a','b','c','l'] "care" == True
-- wordFitsTemplate "??r?" ['c','x','e','w','b','c','l'] "care" == False
-- wordFitsTemplate "??r?" ['c','x','e','a','b','c','l'] "car" == False
-- wordFitsTemplate "??r" ['c','x','e','a','b','c','l'] "care" == False
-- wordFitsTemplate "let" ['x','x'] "let" == True

wordsFittingTemplate :: Template -> Hand -> [String]
wordsFittingTemplate t h = filter (wordFitsTemplate t h) allWords

-- wordsFittingTemplate "??r?" ['c','x','e','a','b','c','l'] == ["acre","bare","carb","care","carl","earl"]


scrabbleValueWord :: String -> Int
scrabbleValueWord word =
    sum $ map scrabbleValue word

-- scrabbleValueWord "care" == 6
-- scrabbleValueWord "quiz" == 22

bestWords :: [String] -> [String]
bestWords ws =
    filter hasMaxValue ws
    where
      maxvalue = maximum (map scrabbleValueWord ws)
      hasMaxValue = (== maxvalue) . scrabbleValueWord

-- bestWords (wordsFittingTemplate "??r?" ['c','x','e','a','b','c','l']) == ["carb"]
-- bestWords ["cat", "rat", "bat"] == ["cat","bat"]
-- bestWords [] == []
