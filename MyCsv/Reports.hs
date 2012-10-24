module MyCsv.Reports
	( regNum
	, printReg
	, compt
	) where
	
import MyCsv.Operations
import MyCsv.Checks
import Data.Char

regNum :: [[String]] -> String -> Int -> IO ()
regNum [] partial num = putStrLn $ ((partial) ++ ", " ++ (show (num-1)))
regNum (x:xs) partial num
	| map toUpper (colVal x 0) == map toUpper partial = regNum xs partial (num+1) 
	| otherwise = printReg partial num xs (colVal x 0) 1
	
printReg :: String -> Int -> [[String]] -> String -> Int -> IO ()
printReg printStr printNum str partial num = do
									putStrLn $ ((printStr) ++ ", " ++ (show printNum))
									regNum str partial num

compt :: [[String]] -> String -> Int -> [String]
compt ([]) word column = [[]]
compt (x:xs) word column = [(checkColVal x word column)] ++ (compt xs word column)