{- 
Program 	:	spr.hs
Author		:	Kaustubh D. Bhalerao
				bhalerao.1@gmail.com
Copyright	:	All rights reserved
Version		: 	Initial draft 
Revisions	: 	Nov 9 - Everything works - barebones structure
Description	:	A commandline program to be used with the Arduino firmware
				for Spreeta Model TSPR170100
				Part of the openBioinstrumentation project
-}

import System.Environment
import Data.List
import System.IO
import System.Directory
import qualified Data.ByteString.Char8 as B
import System.Hardware.Serialport
import Data.List.Split
import Control.Concurrent

-- | Dispatcher data structure
dispatch :: [(String, [String] -> IO ())]
dispatch = [ ("find", findspr) 
            ,("read", readspr)
            ,("calibrate", calibrate)
            ,("help", help)
            ]

main = do
	args <- getArgs
	if (args == []) 
    	then help []
    	else do
    		let	(command:params) = args
    			disp = lookup command dispatch
    		case disp of 
        		Just action -> action params
        		Nothing -> print $ "Available commands - find, read, calibrate, help" 
        
-- | Help - just prints some helpful stuff
help :: [String] -> IO ()
help _ = do
		putStrLn "Simple command line interface for the Spreeta SPR firmware on an Arduino"
		putStrLn "Typical usage: spr [command] [options]"
		putStrLn "Available Commands"
		putStrLn "find - find all available /dev/cu.* devices"
		putStrLn "read [FilePath] - get a single reading from a given filepath"
		putStrLn "\te.g. spr read \"/dev/cu.usbmodem1421\""
		putStrLn "calibrate [FilePath] - get the average voltage across the sensor for the current calibration"
		putStrLn "calibrate [FilePath, value] - set a new calibration level. (value in (0,100))"
		putStrLn "\te.g. spr calibrate \"/dev/cu.usbmodem1421\" 50"
		putStrLn " ----- "
		putStrLn "Most likely you'll need to redirect your reads to a file..."
                 

-- | Find USB devices function                      
findspr :: [FilePath] -> IO ()
findspr [] = findspr ["/dev/"]         
findspr [filepath] = 
    let printfilter dir = print $ filter (isPrefixOf "cu.") dir
    in getDirectoryContents filepath >>= printfilter

-- | Read a single reading from the serial port until terminator is seen      
recursiveReadUntil :: SerialPort -> B.ByteString -> B.ByteString -> IO B.ByteString
recursiveReadUntil s terminator acc = do
    recd <- recv s 10
    if (terminator `B.isSuffixOf` acc)
        then return acc 
        else recursiveReadUntil s terminator $ B.append acc recd

-- | Read a single reading from the serial port
getSingleRead :: FilePath -> IO [String]
getSingleRead path = do
    -- putStrLn "Opening " ++ path
    s <- openSerial path defaultSerialSettings 
    threadDelay 2000000 -- delay necessary to allow arduino to reboot
    send s $ B.pack "1"
    acc <- recursiveReadUntil s (B.pack "OK\r\n") B.empty 
    --closeSerial s
    return $ (lines . B.unpack) acc


-- | Checks if the filepath exists, and prints a single reading
readspr :: [FilePath] -> IO ()
readspr [path] = do 
    exists <- doesFileExist path 
    case exists of  
        False -> putStrLn "No device found"
        True -> getSingleRead path >>= (mapM_ putStrLn) 

-- | Calibration 
calibrate :: [String] -> IO ()
calibrate (path:[]) = do 
    exists <- doesFileExist path
    case exists of 
        False -> putStrLn "No device found"
        True -> do 
            reading_vector <- getSingleRead path
            let readingframe = parse reading_vector
            putStrLn $ show (avg readingframe)
calibrate (path:val:[]) = do 
    exists <- doesFileExist path
    case exists of 
        False -> putStrLn "No device found"
        True -> do 
            putStrLn $ "Opening " ++ path
            s <- openSerial path defaultSerialSettings { timeout = 10 }
            threadDelay 2000000
            send s $ B.pack "3"
            acc <- recursiveReadUntil s (B.pack "?\r\n") B.empty 
            putStrLn $ (B.unpack acc) ++ "%"
            putStrLn $ "Setting new value to " ++ (show val) ++ "%"
            send s $ B.pack val
            acc <- recursiveReadUntil s (B.pack "OK\r\n") B.empty 
            putStrLn "Reacquiring data"
            getSingleRead path
            putStrLn "New average value" 
            calibrate [path]
    

-- Data structure to define a reading frame - consists of two records
-- a Time stamp and a list of voltage values

data ReadingFrame = ReadingFrame { timestamp :: Int
                                 , values :: [(Int,Double)]
                                 , avg :: Double
                                 } deriving (Show)

parse :: [String] -> ReadingFrame
parse list = parse_single list (ReadingFrame { timestamp = 0, values = [], avg = 0 })

parse_single :: [String] -> ReadingFrame -> ReadingFrame
parse_single [] acc = let volts = [v | (_,v) <- values acc] 
                          average = realToFrac (sum volts) / genericLength volts 
                      in ReadingFrame { timestamp = timestamp acc
                                      , values = values acc
                                      , avg = average
                                      }
parse_single (x:xs) acc = 
    if ("T: " `isPrefixOf` x) 
        then let ts = read (dropWhile (/=' ') x) :: Int
             in parse_single xs $ ReadingFrame { timestamp = ts
                                               , values = (values acc)
                                               , avg = (avg acc)
                                               }
        else if (':' `elem` x) 
                then let [idx,volts] = splitOn ":" x 
                    in parse_single xs $ ReadingFrame { timestamp = timestamp acc
                                               , values = (values acc) ++ 
                                                    [(read idx :: Int, read volts :: Double)]
                                               , avg = avg acc
                                               }
                else parse_single xs acc

                            