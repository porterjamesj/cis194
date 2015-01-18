{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

-- TODO this is a very shitty implementation, I would love to improve
-- it
parseMessage :: String -> LogMessage
parseMessage msg = case words msg of
  ("E":num:ts:rs) -> LogMessage (Error (read num)) (read ts) (unwords rs)
  ("I":ts:rs) -> LogMessage Info (read ts) (unwords rs)
  ("W":ts:rs) -> LogMessage Warning (read ts) (unwords rs)
  ws -> Unknown $ unwords ws

parse :: String -> [LogMessage]
parse f =  map parseMessage $ lines f

earlier :: LogMessage -> LogMessage -> Bool
earlier (LogMessage _ ts1 _) (LogMessage _ ts2 _) = ts1 < ts2
earlier (Unknown _) _ = error "can't compare Unknown"
earlier _ (Unknown _) = error "can't compare Unknown"

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert msg tree = case tree of
  Leaf -> Node Leaf msg Leaf
  Node left msg2 right -> if earlier msg msg2
                          then Node (insert msg left) msg2 right
                          else Node left msg2 (insert msg right)

build :: [LogMessage] -> MessageTree
build msgs = foldr insert Leaf msgs

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left msg right) = inOrder left ++ [msg] ++ inOrder right

isProblem :: LogMessage -> Bool
isProblem (LogMessage (Error sev) _ _) = sev > 50
isProblem _ = False

-- XXX I am annoyed that I even have to write this function
getMsg :: LogMessage -> String
getMsg (Unknown _) = error "Can't extract message from Unknown"
getMsg (LogMessage _ _ s) = s

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong msgs = map getMsg $ filter isProblem $ inOrder $ build msgs
