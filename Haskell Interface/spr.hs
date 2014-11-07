-- spr.hs
-- Spreeta interface in Haskell FTW!

import System.Environment
import Data.List
import System.IO
import System.Directory
import qualified Data.ByteString.Char8 as B
import System.Hardware.Serialport
import Data.List.Split

dispatch :: [(String, [String] -> IO ())]
dispatch = [ ("find", findspr) 
            ,("read", readspr)
            ,("calibrate", calibrate)
            ]

main = do
    (command:args) <- getArgs
    let disp = lookup command dispatch
    case disp of 
    	Just action -> action args
        Nothing -> print $ "No such command: " ++ command
                 

-- Find USB devices function                      
findspr :: [FilePath] -> IO ()
findspr [] = findspr ["/dev/"]         
findspr [filepath] = 
	let printfilter dir = print $ filter (isPrefixOf "tty.usb") dir
	in getDirectoryContents filepath >>= printfilter

-- Read a single reading from the serial port
recursiveread :: SerialPort -> B.ByteString -> IO B.ByteString
recursiveread s acc = do 
    recd <- recv s 1000
    print recd
    case (B.null recd) of 
    	True -> return acc
        False -> recursiveread s $ B.append acc recd

-- Read a single reading from the serial port
getSingleRead :: FilePath -> IO [String]
getSingleRead path = do
	s <- openSerial path defaultSerialSettings
	send s $ B.pack "1"
	acc <- recursiveread s B.empty
	closeSerial s
	return $ (lines . B.unpack) acc


-- Checks if the filepath exists, and prints a single reading
readspr :: [FilePath] -> IO ()
readspr [path] = do 
    exists <- doesFileExist path 
    case exists of  
    	False -> putStrLn "No device found"
        True -> getSingleRead path >>= print 

-- Calibration 
calibrate :: [String] -> IO ()
calibrate (path:[]) = do 
	exists <- doesFileExist path
	case exists of 
		False -> putStrLn "No device found"
		True -> do 
			reading_vector <- getSingleRead path
			let readingframe = parse reading_vector
			putStrLn $ show (avg readingframe)
calibrate (path:val:[]) = undefined

-- Data structure to define a reading frame - consists of two records
-- a Time stamp and a list of voltage values

data ReadingFrame = ReadingFrame { timestamp :: Int
								 , values :: [Double]
								 , avg :: Double
								 } deriving (Show)

parse :: [String] -> ReadingFrame
parse list = parse_single list (ReadingFrame { timestamp = 0, values = [], avg = 0 })

parse_single :: [String] -> ReadingFrame -> ReadingFrame
parse_single [] acc = acc
parse_single (x:xs) acc = case x of
	"T:" `isPrefixOf` x -> parse_single xs 
							

--			let volts = [(\x-> let [_,v] = splitOn ":" x 
--							   in read v::Double) x| x <- reading_vector]
--			let avg = realToFrac (sum volts) / genericLength volts