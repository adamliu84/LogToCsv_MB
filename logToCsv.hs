import Data.List.Split
import Data.List
import Data.String.Utils
import Data.Char (isSpace)
import System.Directory

logfilename = "sample.log"
csvfilename = "data.csv"
 
-- https://en.wikipedia.org/wiki/Trimming_%28computer_programming%29#Haskell
trim      :: String -> String
trim      = f . f
	where f = reverse . dropWhile isSpace

-- Register information
getStartingRegister szFirstLine = getRegisterInfo "StartingRegister:" szFirstLine
getNumberOfRegister szFirstLine = getRegisterInfo "NumberOfRegister:" szFirstLine
getRegisterInfo szSpliter szString = read ( (splitOn " " $ trim $ (splitOn szSpliter szString)!!1)!!0)::Int

-- Data information
convertSzData lszRaw =
	(getTimeStamp $ lszRaw!!0,
	getStatus $ lszRaw!!1,
	getRegisterData $ lszRaw!!2)
getTimeStamp x = (splitOn " INFO " x)!!0
getStatus x = (splitOn "StatusOn: " x)!!1
getRegisterData x = replace ", " ";" $ (splitOn "INFO [" x)!!1

-- CSV writer
writeCSVHeader (nStartingRegister, nNumberOfRegister) = do
    appendFile csvfilename $ "TimeStamp;StatusOn;"++(intercalate ";" $ map (\x-> "Register_"++show x) [nStartingRegister..(nStartingRegister+nNumberOfRegister-1)]) ++ "\n"
writeCSVData (x,y,z) = do
    appendFile csvfilename $ x++";"++y++";"++z++"\n"
	
-- Main
main = do
	bExistingCsvFile <- doesFileExist csvfilename
	case bExistingCsvFile of
		True -> removeExistingFile
		_ -> return ()
	tStartAndNoRegister <- (\x -> return (getStartingRegister x, getNumberOfRegister x)) =<< (\x -> return ((splitOn "\n" x)!!0)) =<< readFile logfilename
	writeCSVHeader tStartAndNoRegister
	lRaw <- (\x -> return $ map convertSzData x) =<< (\x -> return $ (map (splitOn "\r\n") x))  =<< (\x -> return.init $ (splitOn "]\r\n" x)) =<< readFile logfilename
	mapM_ writeCSVData lRaw
	print "FIN"
	where removeExistingFile = do 
			           removeFile csvfilename
				   print "Removed existing CSV file"
