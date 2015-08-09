module SqlExecutor
(
    SqlResult (SqlResult, status, resultTable),
    SqlRow,
    executeSqlCommand
) where

import SqlCommandParser
import SqlTable

import Control.Monad

import Data.List.Split

import Data.List
import Data.Maybe
import System.Directory
import System.Environment
import qualified System.IO.Strict as Strict

data SqlResult = SqlResult { status :: String, resultTable :: Maybe SqlTable} deriving (Show)

executeSqlCommand :: ParsedSqlCommand -> IO SqlResult
executeSqlCommand (ParsedSelectSqlCommand fs ts) = executeSelectCommand (ParsedSelectSqlCommand fs ts)
executeSqlCommand (ParsedInsertSqlCommand ts vs) = executeInsertCommand (ParsedInsertSqlCommand ts vs)
executeSqlCommand (ParsedCreateSqlCommand t s cs) = executeCreateCommand (ParsedCreateSqlCommand t s cs)

executeCreateCommand :: ParsedSqlCommand -> IO SqlResult
executeCreateCommand pcc = do
    createDirectoryIfMissing True "tables"
    let pathName = "tables/" ++ (ctable pcc)
    writeFile pathName $ (ctable pcc) ++ "\n"
    appendFile pathName $ (intercalate "," $ map show (cschema pcc)) ++ "\n"
    appendFile pathName $ (intercalate "," (ccols pcc)) ++ "\n"
    return SqlResult {
      status="Success",
      resultTable=Nothing
    }

executeInsertCommand :: ParsedSqlCommand -> IO SqlResult
executeInsertCommand pic = do
    let pathName = "tables/" ++ itable pic
    appendFile pathName $ (intercalate "," $ map show (ivalues pic)) ++ "\n"
    return SqlResult {
      status="Success",
      resultTable=Nothing
    }

selectFromRow :: [Int] -> SqlRow -> Maybe SqlRow
selectFromRow idxs (SqlRow rs) = do
    guard (all (\idx -> idx < length rs) idxs)
    Just $ SqlRow $ map (\idx -> rs !! idx) idxs

selectFromTable :: [String] -> SqlTable -> Maybe SqlTable
selectFromTable cs t = do
    let maybeIdxsToSelect = map ((flip elemIndex) (sqlTableColumns t)) cs
    guard (all isJust maybeIdxsToSelect) -- Not asking for any missing columns
    let idxsToSelect = map fromJust maybeIdxsToSelect
    guard (all (\idx -> idx < (length (sqlTableSchema t))) idxsToSelect)
    let schema = map (\idx -> (sqlTableSchema t) !! idx) idxsToSelect
    let resultRows = map fromJust $ filter isJust $ map (selectFromRow idxsToSelect) (sqlTableRows t)
    Just SqlTable { sqlTableName="", sqlTableSchema=schema, sqlTableColumns=cs, sqlTableRows=resultRows}



executeSelectCommand :: ParsedSqlCommand -> IO SqlResult
-- For now, just select everything and make strings
executeSelectCommand psc = do
    let pathName = "tables/" ++ stable psc
    let fields = sfields psc
    fileContents <- Strict.readFile pathName
    let maybeTable = parseTable fileContents >>= \t -> selectFromTable fields t
    return $ if isJust maybeTable
        then SqlResult {
          status="Error",
          resultTable=Nothing
        }
        else SqlResult { status="Success", resultTable=maybeTable }