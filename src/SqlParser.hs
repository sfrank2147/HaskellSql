module SqlParser
(
    ParsedSqlCommand (
        ParsedSelectSqlCommand, sfields, stable,
        ParsedInsertSqlCommand, itable, ivalues,
        ParsedCreateSqlCommand, ctable, cschema, ccols
    ),
    SqlValue (SqlInt, SqlString),
    SqlValueType (SqlIntType, SqlStringType)
    --parseCommand,
) where

import Data.List
import Data.List.Split

data SqlValueType = SqlIntType | SqlStringType deriving (Enum, Read, Show, Eq)

data SqlValue = SqlInt Int | SqlString String
instance Show SqlValue where
    show (SqlInt i) = show i
    show (SqlString s) = show s

data ParsedSqlCommand = ParsedSelectSqlCommand { sfields :: [String], stable :: String } |
                        ParsedInsertSqlCommand { itable :: String, ivalues :: [SqlValue] } |
                        ParsedCreateSqlCommand { ctable :: String, cschema :: [SqlValueType], ccols :: [String]} deriving (Show)

--parseTableForSelect :: String -> Maybe String
--parseTableForSelect c =
--    let tokens = splitOn " " c
--    in elemIndex "FROM" tokens >>= \idx ->
--       if length tokens <= idx then Nothing else Just (tokens !! (idx + 1))

--parseFieldsForSelect :: String -> Maybe [String]
---- Parse the fields we're selecting out of the SQL command.
---- This function is extremely fragile: all fields must be comma separated
---- with not spaces in between.
--parseFieldsForSelect c =
--    let tokens = splitOn " " c
--    in elemIndex "SELECT" tokens >>= \idx ->
--       if length tokens <= idx then Nothing else Just (splitOn "," (tokens !! (idx + 1)))


--parseSelectCommand :: String -> Maybe ParsedSqlCommand
--parseSelectCommand c =
--    parseFieldsForSelect c >>= \fields ->
--    parseTableForSelect c >>= \table ->
--    Just ParsedSelectSqlCommand {sfields=fields, stable=table}

--parseTableForInsert :: String -> Maybe String
--parseTableForInsert c =
--    let tokens = splitOn " " c
--    in elemIndex "INTO" tokens >>= \idx ->
--       if length tokens <= idx then Nothing else Just (tokens !! (idx + 1))

--parseValuesForSelect :: String -> Maybe [SqlValue]
--parseValuesForSelect c =
--    let tokens = splitOn " " c
--    in elemIndex "VALUES" tokens >>= \idx ->
--       if length tokens <= idx then Nothing else Just $ map SqlString (splitOn "," (tokens !! (idx + 1)))

--parseInsertCommand :: String -> Maybe ParsedSqlCommand
--parseInsertCommand c =
--    parseTableForInsert c >>= \table ->
--    parseValuesForSelect c >>= \values ->
--    Just ParsedInsertSqlCommand {itable=table, ivalues=values}

--parseCommand :: String -> Maybe ParsedSqlCommand
--parseCommand c =
--    let tokens = splitOn  " " c
--    in if length tokens == 0 then Nothing else
--        if (tokens !! 0) == "SELECT" then parseSelectCommand c else
--            if (tokens !! 0) == "INSERT" then parseInsertCommand c else
--                Nothing
