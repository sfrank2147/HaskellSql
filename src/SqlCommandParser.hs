module SqlCommandParser
(
    ParsedSqlCommand (
        ParsedSelectSqlCommand, sfields, stable,
        ParsedInsertSqlCommand, itable, ivalues,
        ParsedCreateSqlCommand, ctable, cschema, ccols
    ),
) where

import SqlTable

import Data.List
import Data.List.Split

data ParsedSqlCommand = ParsedSelectSqlCommand { sfields :: [String], stable :: String } |
                        ParsedInsertSqlCommand { itable :: String, ivalues :: [SqlValue] } |
                        ParsedCreateSqlCommand { ctable :: String, cschema :: [SqlValueType], ccols :: [String]} deriving (Show)
