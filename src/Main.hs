{-# LANGUAGE OverloadedStrings #-}
module Main where

import SqlCommandParser
import SqlExecutor
import SqlTable

import Control.Monad
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Strings
import System.IO
import qualified System.IO.Strict as Strict

printPrompt :: String -> IO ()
printPrompt p = putStr p >> hFlush stdout

parseSelect :: IO ParsedSqlCommand
parseSelect = do
    printPrompt "Table: "
    table <- getLine
    printPrompt "Fields (comma-separated with no spaces): "
    fieldString <- getLine
    let fields = splitOn "," fieldString
    return ParsedSelectSqlCommand {sfields = fields, stable = table}

parseInsert :: IO ParsedSqlCommand
parseInsert = do
    printPrompt "Table name: "
    table <- getLine
    printPrompt "Values (comma-separated with no spaces): "
    valuesString <- getLine
    -- This needs to be refactored into the executor file
    tableFileContents <- Strict.readFile ("tables/" ++ table)
    let schema = fromJust $ parseSchemaForTable tableFileContents
    let valuesList = splitOn "," valuesString
    let values = map (\idx -> stringToSqlValue (schema !! idx) (valuesList !! idx)) [0..(length schema - 1)]
    return ParsedInsertSqlCommand {itable = table, ivalues = values}
  where stringToSqlValue t s = (if t == SqlIntType then SqlInt (read s) else SqlString s)

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
    putStrLn $ "Parsed command:" ++ (show parsedCommand)
    result <- if isJust parsedCommand
        then executeSqlCommand (fromJust parsedCommand)
        else return SqlResult {status="Failure", resultTable=Nothing}
    putStrLn $ show result