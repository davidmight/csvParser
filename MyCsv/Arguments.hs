module MyCsv.Arguments
	( parseArg
	, parseZip
	, split) where

import MyCsv.Operations

--parse the arguments into a list of tuples
parseArg :: [String] -> [(Int, String)]
parseArg [] = []
parseArg (x:[]) = parseZip (split x '=')
parseArg (x:xs) = parseZip (split x '=') ++ parseArg xs

parseZip :: [String] -> [(Int, String)]
parseZip (x:xs) = [((columnToInt x), head xs)]

split :: String -> Char -> [String]
split [] delim = [""]
split (c:cs) delim
   | c == delim = "" : rest
   | otherwise = (c : head rest) : tail rest
   where
       rest = split cs delim