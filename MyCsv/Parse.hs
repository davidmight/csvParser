module MyCsv.Parse
	( csvFormat
	, csvParseString
	, csvParseField
	, insertQuots
	, parseBody
	, parseString
	, quotParse 
	) where

-- PARSE INTO CSV
csvFormat :: [[String]] -> [String]
csvFormat (x:[]) = [(csvParseString (x))]
csvFormat (x:xs) = [(csvParseString (x))] ++ csvFormat xs

csvParseString :: [String] -> String
csvParseString (x:[]) = csvParseField x
csvParseString (x:xs) = csvParseField x ++ [','] ++ csvParseString xs 

csvParseField :: String -> String
csvParseField field = if elem ',' field
						then insertQuots field
						else field
	
insertQuots :: String -> String
insertQuots str = ['"'] ++ str ++ ['"']
	
-- PARSE FROM CSV
parseBody :: [String] -> [[String]] -> [[String]]
parseBody (x:[]) result = (result  ++ ([parseString x [] [[]]]))
parseBody (x:xs) result = parseBody xs (result ++ ([parseString x [] [[]]]))	
	
parseString :: String -> String -> [String] -> [String]
parseString (x:[]) partial result = (result ++ [partial])
parseString (x:xs) partial result
	| x == ',' = parseString xs "" (result ++ [partial])
	| x == '"'  = quotParse xs "" (result)
	| otherwise = parseString xs (partial ++ [x]) (result)

quotParse :: String -> String -> [String] -> [String]
quotParse (x:xs) partial result
	| x == '"' = parseString xs partial (result)
	| otherwise = quotParse xs (partial ++ [x]) (result)