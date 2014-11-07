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
                 

-- Show only files beginning with "tty.usb"
printfilter :: [FilePath] -> IO ()
printfilter dir = print $ filter (isPrefixOf "tty.usb") dir

-- Find USB devices function                      
findspr :: [FilePath] -> IO ()
findspr [] = findspr ["/dev/"]         
findspr [filepath] = getDirectoryContents filepath >>= printfilter

-- Read a single reading from the serial port

recursiveread :: SerialPort -> B.ByteString -> IO B.ByteString
recursiveread s acc = do 
    recd <- recv s 1000
    print recd
    case (B.null recd) of 
    	True -> return acc
        False -> recursiveread s $ B.append acc recd

-- Read a single reading from the serial port
getSingleRead :: FilePath -> IO [Char]
getSingleRead path = do
	s <- openSerial path defaultSerialSettings
	send s $ B.pack "1"
	acc <- recursiveread s B.empty
	return (B.unpack acc)


-- Checks if the filepath exists, and prints a single reading
readspr :: [FilePath] -> IO ()
readspr [path] = do 
    exists <- doesFileExist path 
    case exists of  
    	False -> putStrLn "No device found"
        True -> getSingleRead path >>= print 

-- Calibration 
calibrate :: [String] -> IO ()
calibrate [path] = do 
	exists <- doesFileExist path
	case exists of 
		False -> putStrLn "No device found"
		True -> do 
			reading_vector <- getSingleRead path
			let readings = parse reading_vector
			let volts = [(\x-> let [_,v] = splitOn ":" x 
							   in read v::Double) x| x <- reading_vector]
			let avg = realToFrac (sum volts) / genericLength volts
			putStrLn avg
calibrate [path:val] = undefined
	


