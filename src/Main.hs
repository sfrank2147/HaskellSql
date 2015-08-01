{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.List
import Data.List.Split
import Data.Maybe
import Data.Strings
import SqlExecutor
import SqlParser
import System.IO

--processEntry :: String -> IO String
--processEntry e =
--    let cs = filter (not . strNull) (splitOn ";" e)
--        parsedCommands = map parseCommand cs
--        resultsToPrint = map formatResult parsedCommands
--    in return (intercalate "\n" resultsToPrint)
--    where formatResult r = if isJust r then show r else "Invalid command."

printPrompt :: String -> IO ()
printPrompt p = putStr p >> hFlush stdout

parseSelect :: IO ParsedSqlCommand
parseSelect = do
    printPrompt "Fields (comma-separated with no spaces): "
    fieldString <- getLine
    let fields = splitOn "," fieldString
    printPrompt "Table: "
    table <- getLine
    return ParsedSelectSqlCommand {sfields = fields, stable = table}

parseInsert :: IO ParsedSqlCommand
parseInsert = do
    printPrompt "Table name: "
    table <- getLine
    printPrompt "Values (comma-separated with no spaces): "
    valuesString <- getLine
    let values = map SqlString $ splitOn "," valuesString  -- For now only support strings.
    return ParsedInsertSqlCommand {itable = table, ivalues = values}

parseCreate :: IO ParsedSqlCommand
parseCreate = do
    printPrompt "Table name: "
    table <- getLine
    printPrompt "Schema (comma-separate with no spaces, possible values are INT and STRING): "
    schemaString <- getLine
    let schema = map stringToValueType $ splitOn "," schemaString
    printPrompt "Column names (comma-separated with no spaces): "
    colNames <- getLine
    let cols = splitOn "," colNames
    return ParsedCreateSqlCommand { ctable=table, cschema=schema, ccols=cols }
  where stringToValueType s = if s == "INT" then SqlIntType else SqlStringType

parseQueryType :: String -> IO (Maybe ParsedSqlCommand)
parseQueryType "SELECT" = do
    c <- parseSelect
    return (Just c)
parseQueryType "INSERT" = do
    c <- parseInsert
    return (Just c)
parseQueryType "CREATE" = do
    c <- parseCreate
    return (Just c)
parseQueryType _ = (return Nothing)

parseEntry :: IO (Maybe ParsedSqlCommand)
parseEntry = do
    printPrompt "Type of query (SELECT, INSERT, CREATE): "
    queryType <- getLine
    parsedCommand <- parseQueryType queryType
    return parsedCommand

main :: IO ()
main = do
    parsedCommand <- parseEntry
    result <- if isJust parsedCommand then executeSqlCommand (fromJust parsedCommand) else return SqlResult {status="Failure", resultRows=[]}
    putStrLn $ show parsedCommand
    putStrLn $ show result