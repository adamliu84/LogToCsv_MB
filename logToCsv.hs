import Data.List.Split
import Data.List
import Data.String.Utils
import Data.Char (isSpace)
import System.Directory
import Data.Maybe

logfilename = "sample.log"
csvfilename = "data.csv"
 
-- https://en.wikipedia.org/wiki/Trimming_%28computer_programming%29#Haskell
trim      :: String -> String
trim      = f . f
	where f = reverse . dropWhile isSpace

-- Register information
getStartAndNumRegister x = do
		case (getRegisterInfo "StartingRegister:" x, getRegisterInfo "NumberOfRegister:" x) of
			(Right x, Right y) -> Just (x,y)
			(_,_) -> Nothing
		where getRegisterInfo szSpliter szString = do
			case szSpliter `isInfixOf` szString of
				True -> Right (read ( (splitOn " " $ trim $ (splitOn szSpliter szString)!!1)!!0)::Int)
			 	_ -> Left "Error in Start/Num Register"

-- Data information
convertSzData lszRaw =	
	case (getRegisterData " INFO " (lszRaw!!0) 0,
	      getRegisterData "StatusOn: " (lszRaw!!1) 1,
              getRegisterData "INFO [" (lszRaw!!2) 1) of
		(Right x, Right y, Right z) -> Just (x,y,replace ", " ";" $ z)
		(_,_,_) -> Nothing 	
	where getRegisterData szSpliter szString nIndex = do
		case szSpliter `isInfixOf` szString of
			True -> Right $ (splitOn szSpliter szString)!!nIndex
			_ -> Left "Error in register data"

-- CSV writer
writeCSVHeader (nStartingRegister, nNumberOfRegister) = do
    appendFile csvfilename $ "TimeStamp;StatusOn;"++(intercalate ";" $ map (\x-> "Register_"++show x) [nStartingRegister..(nStartingRegister+nNumberOfRegister-1)]) ++ "\n"
writeCSVData (x,y,z) = do
    appendFile csvfilename $ intercalate  ";" [x,y,z] ++ "\n"
	
-- Main
main = do
	bExistingCsvFile <- doesFileExist csvfilename
	case bExistingCsvFile of
		True -> removeExistingFile
		_ -> return ()
	tStartAndNoRegister <- (\x -> return (getStartAndNumRegister x)) =<< (\x -> return ((splitOn "\n" x)!!0)) =<< readFile logfilename
	case tStartAndNoRegister of
		Nothing -> print "Unable delect either Start or Number of register"
		Just x 	-> do writeCSVHeader x				
			      lRaw <- (\x -> return $ catMaybes $ map convertSzData x) =<< (\x -> return $ (map (splitOn "\r\n") x))  =<< (\x -> return.init $ (splitOn "]\r\n" x)) =<< readFile logfilename			      
			      mapM_ writeCSVData lRaw
	print "FIN"
	where removeExistingFile = do 
			           removeFile csvfilename
				   print "Removed existing CSV file"
