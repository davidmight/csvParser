-- Name: David Byrne
-- Student Id: 09068783

import System.Environment   
import System.Directory  
import System.IO  
import System.IO.Error
import Data.List
import Data.Char

import MyCsv.Parse
import MyCsv.Operations
import MyCsv.Checks
import MyCsv.Cleanup
import MyCsv.Reports
import MyCsv.Formatting
import MyCsv.Arguments
  
-- list of commands possible at the first stage of the project
dispatchOne :: [(String, [String] -> IO ())]  
dispatchOne = [ ("load", load)  
			, ("help", help1)
			, ("quit", quit1)
            ]

-- list of commands possible at the second stage of the project
dispatchTwo :: [(String, [[String]] -> [[String]] -> [String] -> IO ())]
dispatchTwo = [ ("save", save)
			, ("report", report)
			, ("count", count)
			, ("list", list)
			, ("distinct", distinct)
			, ("datefix", datefix)
			, ("gridfix", gridfix)
			, ("reformat", reformat)
			, ("select", select)
			, ("show", showcsv)
			, ("update", update)
			, ("delete", deletecsv)
			, ("insert", insertcsv)
			, ("help", help2)
		    , ("quit", quit2)
			]
	
--Prompt the user for input, seperate the command from the arguments
--Then check the command list for that command
--if it is there use it (passing the arguments) otherwise it is not a valid command	
main = do  
	putStrLn "\nLoad a file:"
	line <- getLine
	let commands = drop 1 (customWords line [] [[]])
	let	commandName = head commands
	let args = drop 1 commands
	if commandListOne commandName dispatchOne
		then do
			let (Just action) = lookup commandName dispatchOne
			action args
		else do 
			if commandListTwo commandName dispatchTwo
				then putStrLn "Must load a file first"
				else putStrLn "Not a valid command"
			main

--In most ways same as the one above only the parsed document is being passed as well as
--the selected fields (which is empty by default)
stageTwo :: [[String]] -> [[String]] -> IO ()
stageTwo info selected = do
	putStrLn "\nPlease enter your command:"
	line <- getLine
	let commands = drop 1 (customWords line [] [[]])
	let commandName = head commands
	let args = (drop 1 commands)
	if commandListTwo commandName dispatchTwo
		then do
			let (Just action) = lookup commandName dispatchTwo
			action info selected args
		else invalid info selected "Not a valid command"
	
--Load a file, firstly try to open it and if that's ok parse the contents, cleanup the results and
--move on to the second stage of the program
load :: [String] -> IO ()
load [fileName] = do
			if validCsv fileName
				then do
					tryOpen fileName `catch` openHandler
					handle <- openFile fileName ReadMode
					contents <- hGetContents handle
					let contentPerLine = lines contents
					let parsedHeader = cleanHeader (parseString (head contentPerLine) [] [[]])
					let fields = length parsedHeader
					let parsedBody = filter (\x -> checkIsEmpty x)  (cleanup (parseBody (tail contentPerLine) [[[]]]) fields)
					putStrLn $ "1 header line (" ++ (show fields) ++ " named fields), " ++ (show (length parsedBody)) ++ " records"
					stageTwo ([parsedHeader] ++ parsedBody) [[]]
				else do
					putStrLn "Not a csv file!"
					main
		
--Parse the information back into its original csv format and writes it into the file given		
save :: [[String]] -> [[String]] -> [String] -> IO ()  
save info selected [fileName] = do
				if validCsv fileName
					then do
						tryOpen fileName `catch` openHandler
						writeFile fileName (unlines (csvFormat info))
						stageTwo info selected
					else invalid info selected "Not a csv file"

--Do one of the in-built reports					
report :: [[String]] -> [[String]] -> [String] -> IO ()  
report info selected [reportType]  
	| reportType == "registrations" = registReport selected info
	| reportType == "completions" = compReport selected info
	| otherwise = invalid info selected "Not an in-built report"

registReport :: [[String]] -> [[String]] -> IO () 
registReport info selected = do
				regNum (tail info) (((tail info) !! 0) !! 0) 0
				stageTwo info selected
				
compReport :: [[String]] -> [[String]] -> IO ()
compReport info selected = do
				printListStr (filter (\x -> length x > 2) (compt (tail info) "completed" 7))
				stageTwo info selected
	
--Parse the arguments into a list of tuples in the form (Int, String)
--the first field being the column and the second field is the value
--Then get the length of result which meets those arguments and the count
count :: [[String]] -> [[String]] -> [String] -> IO ()
count info selected args = do
					let parsedArgs = parseArg args
					putStrLn (show (length (filter (\x -> checkIsEmpty x) (rowConds info parsedArgs))) ++ " values for these arguments")
					stageTwo info selected

--Same as count only in this it prints out the result
list :: [[String]] -> [[String]] -> [String] -> IO ()
list info selected args = do
					let parsedArgs = parseArg args
					print (filter (\x -> checkIsEmpty x) (rowConds info parsedArgs))
					stageTwo info selected

--Gets the number of distinct values in a column by grouping all the various
--fields in a particular column and getting the length of it
distinct :: [[String]] -> [[String]] -> [String] -> IO ()  
distinct selected info [column] = if checkColumn (column) info
									then do
										let distinctLen = length (group (getColumn (tail info) (columnToInt column)))
										putStrLn ((show distinctLen) ++ " distinct values for " ++ (show (colVal (head info) (columnToInt column))))
										stageTwo info selected
									else invalid info selected "Not a valid column"
	
--Didn't manage to finish this command	
datefix :: [[String]] -> [[String]] -> [String] -> IO ()  
datefix info selected [column, format] = if checkColumn (column) info
											then if checkDateFormat format
												then putStrLn "Not complete"
												else invalid info selected "Not a valid date format"
											else invalid info selected "Not a valid column"

--Didn't manage to finish this command	
gridfix :: [[String]] -> [[String]] -> [String] -> IO ()  
gridfix info selected [column, format] = if checkColumn (column) info
											then if checkGridFormat format
												then putStrLn "Not complete"
												else invalid info selected "Not a valid grid format"
											else invalid info selected "Not a valid column"

--Gets a list of fields in that particular column and performs one of the
--transformations on it. That column is then passed back into a new [[String]]
reformat :: [[String]] -> [[String]] -> [String] -> IO ()  
reformat info selected [column, format] = if checkColumn (column) info
											then if checkReformat format
												then do
													let transformed = formatColumn (tail info) (columnToInt column) format
													putStrLn (show (length transformed) ++ " records adjusted")
													let custom = formatResult (tail info) transformed (columnToInt column)
													stageTwo ([(head info)] ++ custom) selected 
												else invalid info selected "Not a valid format"
											else invalid info selected "Not a valid column"

--Gets all the rows that meet the arguments and passes that back into the second stage
--of the program
select :: [[String]] -> [[String]] -> [String] -> IO ()
select info selected args = do
					let parsedArgs = parseArg args
					let selectFields = (filter (\x -> checkIsEmpty x) (rowConds info parsedArgs))
					stageTwo info selectFields

--Prints the contents of the previous select. If there wasn't a select it doesn't
showcsv :: [[String]] -> [[String]] -> [String] -> IO ()  
showcsv info selected [] = if checkSelect selected
							then do
								mapM_ print selected
								stageTwo info selected
							else invalid info selected "You have not selected anything"

--Changes the value of a particular field when given a row and column
update :: [[String]] -> [[String]] -> [String] -> IO ()  
update info selected [row, column, value] = if checkRow row info
												then if (elem (column) (head info))
													then do
														let custom = changeRow (tail info) value (read row::Int) (columnToInt column)
														putStrLn "The value has been changed"
														stageTwo custom selected
													else invalid info selected "Not valid column"
												else invalid info selected "Not a valid row"
											
--Deletes a particular row
deletecsv :: [[String]] -> [[String]] -> [String] -> IO ()  
deletecsv info selected [row] = if checkRow row info
									then do
										let custom = deleteRow (tail info) (read row::Int)
										putStrLn $ "Row " ++ (row) ++ " has been deleted"
										stageTwo custom selected
									else invalid info selected "Not a valid row"

--Inserts a new row with arguments as the new fields
insertcsv :: [[String]] -> [[String]] -> [String] -> IO ()
insertcsv info selected args = do
					let parsedArgs = parseArg args
					let custom = insertRow (info ++ [(take (length (head info)) (repeat ""))]) parsedArgs
					stageTwo custom selected

--The quit for the first stage
quit1 :: [String] -> IO ()  
quit1 [] = do
		putStrLn "The program is ending!\n"
		return ()
		
--The quit for the second stage
quit2 :: [[String]] -> [[String]] -> [String] -> IO ()  
quit2 info selected [] = do
		putStrLn "The program is ending!\n"
		return ()

--The help for the first stage	
--Gives a list of commands	
help1 :: [String] -> IO ()  
help1 [] = do
		printHelpInfo
		main
	
--The help for the second stage	
help2 :: [[String]] -> [[String]] -> [String] -> IO ()  
help2 info selected [] = do
		printHelpInfo
		stageTwo info selected
		
tryOpen :: String -> IO ()
tryOpen fileName = do
				handle <- openFile fileName ReadMode
				contents <- hGetContents handle
				hClose handle
				
openHandler :: IOError -> IO ()
openHandler e
	| isDoesNotExistError e = linkBack "This file does not exist"
	| otherwise = ioError e	

invalid :: [[String]] -> [[String]] -> String -> IO ()
invalid info selected explain = do
			putStrLn explain
			stageTwo info selected
	
linkBack :: String -> IO()
linkBack explain = do
		putStrLn explain
		main
		
commandListOne :: String -> [(String, [String] -> IO ())] -> Bool
commandListOne commandName dispatch = elem  commandName (fst (unzip dispatch))
		
commandListTwo :: String -> [(String,[[String]] -> [[String]] -> [String] -> IO ())] -> Bool
commandListTwo commandName dispatch = elem  commandName (fst (unzip dispatch))

--Function for parsing the user input as line was giving me enough control
customWords :: String -> String -> [String] -> [String]
customWords (x:[]) partial result = (result ++ [partial ++ [x]])
customWords (x:xs) partial result
	| x == ' ' = customWords xs "" (result ++ [partial])
	| x == '"'  = customWordsQuots xs partial (result)
	| otherwise = customWords xs (partial ++ [x]) (result)

customWordsQuots :: String -> String -> [String] -> [String]
customWordsQuots (x:[]) partial result = (result ++ [partial])
customWordsQuots (x:xs) partial result
	| x == '"' = customWords xs partial (result)
	| otherwise = customWordsQuots xs (partial ++ [x]) (result)
		
printHelpInfo :: IO ()
printHelpInfo = do		
		putStrLn "load - load CSV spreadsheet"
		putStrLn "save - save spreadsheet as CSV"
		putStrLn "report - run builtin report"
		putStrLn "count - count records satisfying a condition"
		putStrLn "list - show records satisfying a condition"
		putStrLn "distinct - report distinct items in a column"
		putStrLn "date-fix - fix date data"
		putStrLn "grid-fix - fix grid-reference data"
		putStrLn "reformat - reformat column data"
		putStrLn "select - select sheet rows"
		putStrLn "show - show the selected rows"
		putStrLn "update - update field"
		putStrLn "delete - delete row"
		putStrLn "insert - insert new row"
		putStrLn "quit - exit the program"