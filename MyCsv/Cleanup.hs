module MyCsv.Cleanup
	( cleanHeader
	, removeItem
	, cleanup
	, clean 
	) where

cleanHeader :: [String] -> [String]
cleanHeader header = removeItem "" header

removeItem :: String -> [String] -> [String]
removeItem _ [] = []
removeItem x (y:ys) | x == y = removeItem x ys
					| otherwise = y : removeItem x ys
	
cleanup :: [[String]] -> Int -> [[String]]
cleanup (x:[]) fieldNum = [(clean x fieldNum)]
cleanup (x:xs) fieldNum = [(clean x fieldNum)] ++ (cleanup xs fieldNum)

clean :: [String] -> Int -> [String]
clean field fieldNum = drop 1 (take (fieldNum+1) field) 