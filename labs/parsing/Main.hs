module Main where

import System.IO
import Log
import Data.List

main :: IO ()
main = processLogFile "error.log" "onlyerrors.log"

readLogFile :: String -> IO [String]
readLogFile path = do 
		   file <- readFile path
		   let fileLines = lines file
		   return fileLines

parseMessage :: String -> MaybeLogMessage
parseMessage line = decodeLine$words line

decodeLine :: [String] -> MaybeLogMessage
decodeLine line = if (head line) == "I" then constructValid Info $ tail line  else if (head line) == "E" then constructError $ tail line else if (head line) == "W" then constructValid Warning $ tail line else InvalidLM (unwords line)

constructValid :: MessageType -> [String] ->MaybeLogMessage
constructValid mType line = ValidLM (LogMessage mType (read (head line) :: Int) (unwords $ tail line))

constructError :: [String] -> MaybeLogMessage
constructError (x:xs) = ValidLM (LogMessage (Error (read$ x::Int)) (read (head xs) :: Int) (unwords (tail xs)))

validMessagesOnly :: [MaybeLogMessage] -> [LogMessage]
validMessagesOnly [] = []
validMessagesOnly (ValidLM x:xs) = x:validMessagesOnly xs
validMessagesOnly (InvalidLM x:xs) = validMessagesOnly xs

parse :: String -> IO [LogMessage]
parse path = do 
             logFile <- readLogFile path
	     let parsed = validMessagesOnly $ map parseMessage logFile
	     return parsed

compareMsgs :: LogMessage -> LogMessage -> Ordering
compareMsgs (LogMessage _ m1 _) (LogMessage _ m2 _) = if m1 - m2 > 0 then GT else if m1 - m2 < 0 then LT else EQ

sortMessage :: [LogMessage] -> [LogMessage]
sortMessage list = sortBy compareMsgs list

whatWentWrong :: [LogMessage] -> [(TimeStamp, String)]
whatWentWrong list = errorCheck $ sortMessage list

errorCheck :: [LogMessage] -> [(TimeStamp, String)]
errorCheck ((LogMessage (Error x) time mess):xs) = if x >= 50 then (time, mess):errorCheck xs else errorCheck xs
errorCheck [] = []
errorCheck ((LogMessage _ _ _):xs) = errorCheck xs

formatError :: (TimeStamp, String) -> String
formatError (time, mess) = "[" ++ show time ++ "] " ++ mess

formatWWW :: [LogMessage] -> [String]
formatWWW list = map formatError (whatWentWrong list)

processLogFile :: String -> String -> IO ()
processLogFile path fileName = do 
                               log <- parse path
                               let errors =formatWWW log
                               writeFile fileName $ unlines errors
                               
