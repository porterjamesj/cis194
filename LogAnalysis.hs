{-# OPTIONS_GHC -Wall #-}
module HW03 where

import Log

data ParseIntermediate = ParseIntermediate MaybeLogMessage

parseMessage :: String -> MaybeLogMessage
parseMessage "" = InvalidLM ""
parseMessage s =
  parseType . parseTimeStamp . parseRest $ s

parseType :: String -> (MaybeLogMessage, MaybeLogMessage)
parseType [] =
