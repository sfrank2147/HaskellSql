module SqlExecutor
(
    SqlResult (SqlResult, status, resultRows),
    SqlRow,
    executeSqlCommand,
) where

import SqlParser

import Data.List.Split

data SqlRow = SqlRow [SqlValue]
data SqlTable = SqlTable { name :: String, schema :: [SqlValueType], tableRows :: [SqlRow]}
data SqlResult = SqlResult { status :: String, resultRows :: [SqlRow]}

executeSqlCommand :: ParsedSqlCommand -> IO SqlResult
executeSqlCommand (ParsedSelectSqlCommand fs ts) = executeSelectCommand (ParsedSelectSqlCommand fs ts)
executeSqlCommand (ParsedInsertSqlCommand ts vs) = executeInsertCommand (ParsedInsertSqlCommand ts vs)

executeSelectCommand :: ParsedSqlCommand -> IO SqlResult
executeSelectCommand psc = return SqlResult {status="Selected", resultRows=[]}

executeInsertCommand :: ParsedSqlCommand -> IO SqlResult
executeInsertCommand pic = return SqlResult {status="Inserted", resultRows=[]}

executeCreateCommand :: ParsedSqlCommand -> IO SqlResult
executeCreateCommand pcc = return SqlResult {status="Created", resultRows=[]}

-- To start: what's the simplest way to store everything?
-- The file stores a list of tables.
-- Each table stores a name, a list of types, a list of rows
-- In the absolute easiest incarnation, everything is in memory.
-- Creating a table adds to the table in the list.

parseSchema :: String -> [SqlValueType]
parseSchema sch =
    let sqlTypeStrings = splitOn "," sch
    in map read sqlTypeStrings

parseRow :: String -> SqlRow
parseRow s =
    let valStrings = splitOn "," s
    in SqlRow (map SqlString valStrings)  -- For now, only support Strings

parseTable :: String -> SqlTable
-- Given the text representation of the table, parse it into a table
parseTable s =
    let n:s:rs = splitOn "\n" s
    in SqlTable {name=n, schema=(parseSchema s), tableRows=(map parseRow rs)}