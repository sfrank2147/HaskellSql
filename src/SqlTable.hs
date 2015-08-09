module SqlTable
(
    SqlValue (SqlInt, SqlString),
    SqlValueType (SqlIntType, SqlStringType),
    SqlRow (SqlRow),
    SqlTable (SqlTable, sqlTableName, sqlTableSchema, sqlTableColumns, sqlTableRows),
    parseTable,
    parseSchema,
    parseSchemaForTable
) where

import Control.Monad
import Data.List
import Data.List.Split
import Data.Maybe
import Util

data SqlValueType = SqlIntType | SqlStringType deriving (Enum, Read, Show, Eq)

data SqlValue = SqlInt Int | SqlString String
instance Show SqlValue where
    show (SqlInt i) = show i
    show (SqlString s) = show s

data SqlRow = SqlRow [SqlValue] deriving (Show)

data SqlTable = SqlTable { sqlTableName :: String, sqlTableSchema :: [SqlValueType], sqlTableColumns :: [String], sqlTableRows :: [SqlRow]} deriving (Show)

getTypeForColumn :: SqlTable -> String -> Maybe SqlValueType
getTypeForColumn t s =
    elemIndex s (sqlTableColumns t) >>= \idx ->
    if idx > length (sqlTableSchema t)
        then Nothing
        else Just $ (sqlTableSchema t) !! idx

parseSchema :: String -> Maybe [SqlValueType]
parseSchema s =
    let schemaStrings = splitOn "," s
        maybeTypes = map maybeRead schemaStrings
    in if all isJust maybeTypes
        then Just (map fromJust maybeTypes)
        else Nothing

stringAndTypeToMaybeVal :: String -> SqlValueType -> Maybe SqlValue
stringAndTypeToMaybeVal s SqlIntType = maybeRead s >>= \i -> Just $ SqlInt i
stringAndTypeToMaybeVal s SqlStringType = maybeRead s >>= \s -> Just $ SqlString s

parseRows :: [SqlValueType] -> [String] -> [Maybe SqlRow]
parseRows schema rows = map (parseRow schema) rows

parseRow :: [SqlValueType] -> String -> Maybe SqlRow
parseRow schema row =
    let rowStrings = splitOn "," row
        maybeVals = map (\(s, t) -> stringAndTypeToMaybeVal s t) $ zip rowStrings schema
    in if all isJust maybeVals
        then Just (SqlRow $ map fromJust maybeVals)
        else Nothing        

parseSchemaForTable :: String -> Maybe [SqlValueType]
-- Parse a list of sql value types from the string representing the table
parseSchemaForTable s = do
    guard (length (lines s) >= 3)
    let n:schema:cs:rs = lines s
    parseSchema schema

parseTable :: String -> Maybe SqlTable
-- Given the text representation of the table, parse it into a table
parseTable s = do
    guard (length (lines s) >= 3)
    let n:s:cs:rs = filter notEmpty $ splitOn "\n" s
    schema <- parseSchema s
    let columns = splitOn "," cs
    guard (length schema == length columns)
    let rows = map fromJust $ filter isJust $ parseRows schema rs
    Just SqlTable {sqlTableName=n, sqlTableSchema=schema, sqlTableColumns=columns, sqlTableRows=rows}
  where notEmpty s = length s > 0