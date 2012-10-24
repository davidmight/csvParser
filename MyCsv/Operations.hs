module MyCsv.Operations
	( changeRow
	, changeColVal
	, rowConds
	, ifRow
	, getColumn
	, colVal
	, insertRow
	, deleteRow
	, columnToInt 
	, printListStr
	) where

changeRow :: [[String]] -> String -> Int -> Int -> [[String]]
changeRow original newval row col =
	let
		(x,y:ys) = splitAt row original
	in x ++ (changeColVal y newval (col-1)) : ys

changeColVal :: [String] -> String -> Int -> [String]
changeColVal original newval col = 
	let 
		(x,_:ys) = splitAt col original
	in x ++ newval : ys 
	
--Gets a list of rows that meet a set of conditions
rowConds :: [[String]] -> [(Int, String)] -> [[String]]
rowConds [] args = []
rowConds (x:[]) args = [(ifRow x args)]
rowConds (x:xs) args = [(ifRow x args)] ++ rowConds xs args

ifRow :: [String] -> [(Int, String)] -> [String]
ifRow row [] = []
ifRow row (x:[])
	| (colVal row (fst x)) /= (snd x) = []
	| otherwise = row
ifRow row (x:xs)
	| (colVal row (fst x)) /= (snd x) = []
	| otherwise = ifRow row xs

--Gets a list of fields in a particular column
getColumn :: [[String]] -> Int -> [String]
getColumn [] col = []
getColumn (x:xs) col = [(colVal x col)] ++ (getColumn xs col)

colVal :: [String] -> Int -> String
colVal str col = (str !! (col-1))
	
insertRow :: [[String]] -> [(Int, String)] -> [[String]]
insertRow info (x:[]) = changeRow info (snd x) ((length info)-1) (fst x)
insertRow info (x:xs) = insertRow (changeRow info (snd x) ((length info)-1) (fst x)) xs  

--Splits the list and then doesn't add the chosen row back into the new spreadsheet
deleteRow :: [[String]] -> Int -> [[String]]
deleteRow original row =
	let
		(x,_:ys) = splitAt (row-1) original
	in x ++ ys

--Converts a column ($1) into a int (1)
--Doesn't have any checks because they should be checked already
columnToInt :: String -> Int
columnToInt column = read (tail column)::Int

printListStr :: [String] -> IO ()
printListStr [] = return ()
printListStr (x:xs) = do 
					putStrLn x
					printListStr xs