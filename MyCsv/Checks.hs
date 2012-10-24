module MyCsv.Checks
	( validCsv
	, checkColVal
	, checkRow
	, checkColumn
	, checkDateFormat
	, checkReformat
	, checkGridFormat
	, checkIsEmpty
	, checkSelect
	) where

import MyCsv.Operations
import Data.Char

validCsv :: String -> Bool
validCsv fileName
	| (take 4 (reverse fileName)) == "vsc." = True
	| otherwise 		   = False
	
checkColVal :: [String] -> String -> Int -> String
checkColVal str word col
	| (str !! col) == word = (str !! 1)
	| otherwise = []

checkRow :: String -> [[String]] -> Bool
checkRow row info
	| (read row::Int) > rows = False
	| (read row::Int) < 0 = False
	| otherwise = True
	where rows = length info

checkColumn :: String -> [[String]] -> Bool
checkColumn column info
	| (head column) /= '$' = False
	| (columnToInt column) > fields || (columnToInt column) <= 0  = False
	| otherwise = True
	where fields = length (head info)

checkDateFormat :: String -> Bool
checkDateFormat format
	| format == "%Y-%M-%D" = True
	| format == "%D-%M-%Y" = True
	| format == "%M-%D-%Y" = True
	| otherwise = False
	
checkReformat :: String -> Bool
checkReformat format
	| (map toLower format) == "uppercase" = True
	| (map toLower format) == "capitalize" = True
	| (map toLower format) == "lowercase" = True
	| otherwise = False
	
checkGridFormat :: String -> Bool
checkGridFormat format
	| format == "4" = True
	| format == "6" = True
	| otherwise = False
	
checkIsEmpty :: [String] -> Bool
checkIsEmpty [] = False 
checkIsEmpty (x:xs) = if (x /= "")
					then True
					else checkIsEmpty xs
					
checkSelect :: [[String]] -> Bool
checkSelect selected
	| selected == [[]] = False
	| otherwise = True