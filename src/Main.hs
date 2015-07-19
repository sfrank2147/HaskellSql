{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.List
import Data.List.Split
import Data.Maybe
import Data.Strings
import SqlParser
import System.IO

processEntry :: String -> IO String
processEntry e =
    let cs = filter (not . strNull) (splitOn ";" e)
        parsedCommands = map parseCommand cs
        resultsToPrint = map formatResult parsedCommands
    in return (intercalate "\n" resultsToPrint)
    where formatResult r = if isJust r then show r else "Invalid command."

main :: IO ()
main = do
    putStr "Command: "
    hFlush stdout  -- Otherwise the program buffers the prompt.
    entry <- getLine
    result <- processEntry entry
    putStrLn result