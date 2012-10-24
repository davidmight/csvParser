module MyCsv.Formatting
	( formatResult
	, formatColumn
	, toCapital
	, capitalise
	, capWord
	, cap
	, stringToUpper
	, stringToLower
	) where
	
import MyCsv.Operations
import Data.Char
	
formatResult :: [[String]] -> [String] -> Int -> [[String]]
formatResult info [] col = []
formatResult (x:xs) (y:ys) col = [changeColVal x y (col-1)] ++ formatResult xs ys col
									
formatColumn :: [[String]] -> Int -> String -> [String]
formatColumn info col format 
	| mat == "uppercase" =  map stringToUpper (getColumn info col)
	| mat == "capitalize" =  toCapital (getColumn info col)
	| mat == "lowercase" =  map stringToLower (getColumn info col)
	| otherwise = getColumn info col
	where mat = map toLower format
	
toCapital :: [String] -> [String]
toCapital [] = []
toCapital (x:xs) = [capitalise x] ++ toCapital xs

capitalise :: String -> String
capitalise str = unwords (capWord (words str))

capWord :: [String] -> [String]
capWord [] = []
capWord (x:xs) = [cap x] ++ capWord xs

cap :: String -> String
cap (x:xs) = toUpper x : xs

stringToUpper		:: String -> String
stringToUpper		= map toUpper

stringToLower		:: String -> String
stringToLower		= map toLower