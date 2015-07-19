module SqlParser
(
    ParsedSqlCommand,
    parseCommand,
) where

import Data.List
import Data.List.Split

data ParsedSqlCommand = ParsedSqlCommand { fields :: [String]
                                           , table :: String
                                         } deriving (Show)

parseTable :: String -> Maybe String
parseTable c =
    let tokens = splitOn " " c
    in elemIndex "FROM" tokens >>= \idx ->
       Just (tokens !! (idx + 1))

parseSelect :: String -> Maybe [String]
-- Parse the fields we're selecting out of the SQL command.
-- This function is extremely fragile: all fields must be comma separated
-- with not spaces in between.
parseSelect c =
    let tokens = splitOn " " c
    in elemIndex "SELECT" tokens >>= \idx ->
       Just (splitOn "," (tokens !! (idx + 1)))  -- Safe, b/c elemIndex returns None on failure


parseCommand :: String -> Maybe ParsedSqlCommand
parseCommand c =
    parseSelect c >>= \fields ->
    parseTable c >>= \table ->
    Just ParsedSqlCommand {fields=fields, table=table}