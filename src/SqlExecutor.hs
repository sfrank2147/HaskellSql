module SqlExecutor
(
    SqlResult (SqlResult, status, resultRows),
    SqlRow,
    executeSqlCommand,
) where

import SqlParser

import Data.List.Split

import Data.List
import System.Directory
import System.Environment

data SqlRow = SqlRow [SqlValue] deriving (Show)
data SqlTable = SqlTable { name :: String, schema :: [SqlValueType], colNames :: [String], tableRows :: [SqlRow]}
data SqlResult = SqlResult { status :: String, resultRows :: [SqlRow]} deriving (Show)

executeSqlCommand :: ParsedSqlCommand -> IO SqlResult
executeSqlCommand (ParsedSelectSqlCommand fs ts) = executeSelectCommand (ParsedSelectSqlCommand fs ts)
executeSqlCommand (ParsedInsertSqlCommand ts vs) = executeInsertCommand (ParsedInsertSqlCommand ts vs)
executeSqlCommand (ParsedCreateSqlCommand t s cs) = executeCreateCommand (ParsedCreateSqlCommand t s cs)

parseToSqlRow :: String -> SqlRow
parseToSqlRow r = SqlRow $ map (SqlString . read) $ splitOn "," r

executeSelectCommand :: ParsedSqlCommand -> IO SqlResult
-- For now, just select everything and make strings
executeSelectCommand psc = do
    let pathName = "tables/" ++ stable psc
    fileContents <- readFile pathName
    let name:schema:colnames:rs = lines fileContents
    return SqlResult { status="Success", resultRows=(map parseToSqlRow rs)}

executeInsertCommand :: ParsedSqlCommand -> IO SqlResult
executeInsertCommand pic = do
    let pathName = "tables/" ++ itable pic
    appendFile pathName $ (intercalate "," $ map show (ivalues pic)) ++ "\n"
    return SqlResult { status="Success", resultRows=[] }

executeCreateCommand :: ParsedSqlCommand -> IO SqlResult
executeCreateCommand pcc = do
    createDirectoryIfMissing True "tables"
    let pathName = "tables/" ++ (ctable pcc)
    writeFile pathName $ (ctable pcc) ++ "\n"
    appendFile pathName $ (intercalate "," $ map show (cschema pcc)) ++ "\n"
    appendFile pathName $ (intercalate "," (ccols pcc)) ++ "\n"
    return SqlResult {status="Success", resultRows=[]}

-- To start: what's the simplest way to store everything?
-- The file stores a list of tables.
-- Each table stores a name, a list of types, a list of column names, a list of rows
-- In the absolute easiest incarnation, everything is in memory.
-- Creating a table adds to the table in the list.
-- But, it turns out inserting into the middle of a file is a pain in the ass.
-- New strategy: different file for each table, stored in a single folder.
-- Format is the same:
-- * first row, name of table
-- * second row, schema
-- * third row, names of columns
-- * subsequent rows are data rows

-- * How will reading the table go?
-- * Get the schema from the 2nd row.
-- * Get the column names from the third row
-- * For each subsequent row:
--    * Split the row into tokens (by comma)
--    * For each value asked for:
--        * Find the index of the value
--        * Find the type of that value from the schema
--        * Parse row[index] according to that type

parseSchema :: String -> [SqlValueType]
parseSchema sch =
    let sqlTypeStrings = splitOn "," sch
    in map read sqlTypeStrings

parseColNames :: String -> [String]
parseColNames = splitOn ","

parseRow :: String -> SqlRow
parseRow s =
    let valStrings = splitOn "," s
    in SqlRow (map SqlString valStrings)  -- For now, only support Strings

parseTable :: String -> SqlTable
-- Given the text representation of the table, parse it into a table
parseTable s =
    let n:s:cs:rs = splitOn "\n" s
    in SqlTable {name=n, schema=(parseSchema s), colNames=(parseColNames cs), tableRows=(map parseRow rs)}