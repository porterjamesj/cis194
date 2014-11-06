{-# OPTIONS_GHC -Wall #-}
module HW03 where

import Log

parseMessage :: String -> MaybeLogMessage
parseMessage "" = InvalidLM ""
parseMessage msg =
  case ws of
   ("E":rest) -> parseError rest
   ("W":rest) -> timeStampMsg Warning rest
   ("I":rest) -> timeStampMsg Info rest
   _ -> InvalidLM msg
   where ws = words msg

parseError :: [String] -> MaybeLogMessage
parseError [] = InvalidLM "E"
parseError (lvl:rest) =
  case readInt lvl of
   ValidInt l -> timeStampMsg (Error l) rest
   InvalidInt -> InvalidLM $ "E " ++ lvl ++ unwords rest

timeStampMsg :: MessageType -> [String] -> MaybeLogMessage
timeStampMsg _ [] = InvalidLM "FIXME"
timeStampMsg msgt (stamp:msg) =
  case readInt stamp of
   ValidInt s -> ValidLM $ LogMessage msgt s (unwords msg)
   InvalidInt -> InvalidLM "FIXME"
