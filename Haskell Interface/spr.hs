{- 
Program     :   spr.hs
Author      :   Kaustubh D. Bhalerao
                bhalerao.1@gmail.com
Copyright   :   All rights reserved
Version     :   Initial draft 
Revisions   :   Nov 9 - Everything works - barebones structure
				Nov 17 - everything compiles - added stream function
					   - made more refinements to code to increase abstraction
				Nov 18 - fixed a bug in recursive read - appending the recd buffer
				         before checking for terminator character. 
Description :   A commandline program to be used with the Arduino firmware
                for Spreeta Model TSPR170100
                Part of the openBioinstrumentation project
-}

--module Spr where

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
            ,("readraw", readspr)
            ,("calibrate", calibrate)
            ,("read", readri)
            ,("stream", stream)
            ,("help", help)
            ]

main = do
    args <- getArgs
    if (args == []) 
        then help []
        else do
            let (command:params) = args
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
        putStrLn "readraw [FilePath] - get a single reading from a given filepath."
        putStrLn "\tOutputs the readings in a raw format. For diagnostic purposes."
        putStrLn "\te.g. spr readraw \"/dev/cu.usbmodem1421\""
        putStrLn "\tYou probably want the 'read' function"
        putStrLn "calibrate [FilePath] - get the average voltage across the sensor for "
        putStrLn "\tthe current calibration"
        putStrLn "calibrate [FilePath, value] - set a new calibration level. (value in (0,100))"
        putStrLn "\te.g. spr calibrate \"/dev/cu.usbmodem1421\" 50"
        putStrLn "read [FilePath] - provides a single reading in a 'Timestamp, RIU' format"
        putStrLn "\te.g. spr read \"/dev/cu.usbmodem1421\""
        putStrLn "stream [FilePath] - stream readings at 1000 ms intervals from the Arduino"
        putStrLn "\te.g. spr stream \"/dev/cu.usbmodem1421\" >> out.txt"
        putStrLn "\tProduces readings in a Timestamp, RIU format"
        putStrLn "Most likely you'll need to redirect your reads to a file like above"
                 

-- | Find USB devices function                      
findspr :: [FilePath] -> IO ()
findspr [] = findspr ["/dev/"]         
findspr [filepath] = 
    let printfilter dir = print $ filter (isPrefixOf "cu.") dir
    in getDirectoryContents filepath >>= printfilter

-- | Read a single reading from the serial port until terminator is seen      
recursiveReadUntil :: SerialPort -> B.ByteString -> B.ByteString -> IO B.ByteString
recursiveReadUntil s terminator acc = do
    recd <- recv s 1000
    let newacc = B.append acc recd
    if (terminator `B.isSuffixOf` newacc)
        then return newacc 
        else recursiveReadUntil s terminator newacc

-- | Read a single reading from the serial port
getSingleRead :: FilePath -> IO [String]
getSingleRead path = do
    -- putStrLn "Opening " ++ path
    s <- openSerial path defaultSerialSettings 
    threadDelay 2000000 -- delay necessary to allow arduino to reboot
    send s $ B.pack "1"
    acc <- recursiveReadUntil s (B.pack "OK\r\n") B.empty 
    closeSerial s
    return $ (lines . B.unpack) acc

-- | Prints a single reading in its raw state
readspr :: [FilePath] -> IO ()
readspr [path] = doIfFileExists path (\x -> do
    frame <- getSingleRead x
    let readingframe = parse frame
    putStrLn $ "Time: " ++ show (timestamp readingframe)
    putStrLn $ "RIU: " ++ show (riu readingframe)
    putStrLn $ "Average voltage: " ++ show (avg readingframe)
    mapM_ (\(x,y) -> putStrLn ((show x) ++ ":" ++ show (y))) $ values readingframe
    ) path


-- | Read a single frame and produce a single RIU reading
readri :: [FilePath] -> IO ()
readri [] = putStrLn "What device?"
readri [path] = doIfFileExists path (\x -> do
    -- putStrLn "Reading"
    frame <- getSingleRead x
    let readingframe = parse frame
    putStrLn $ (show (timestamp readingframe)) ++ ", " ++ (show (riu readingframe))) path
                        
-- | Calibration 
calibrate :: [String] -> IO ()
calibrate (path:[]) = doIfFileExists path (\x -> do
    reading_vector <- getSingleRead x
    let readingframe = parse reading_vector
    putStrLn $ show (avg readingframe)) path
            
calibrate (path:val:[]) = doIfFileExists path (\[x,y] -> do
    putStrLn $ "Opening " ++ x
    s <- openSerial x defaultSerialSettings { timeout = 10 }
    threadDelay 2000000
    send s $ B.pack "3"
    acc <- recursiveReadUntil s (B.pack "?\r\n") B.empty 
    putStrLn "Back"
    putStrLn $ (B.unpack acc) ++ "%"
    putStrLn $ "Setting new value to " ++ (show y) ++ "%"
    send s $ B.pack y
    acc <- recursiveReadUntil s (B.pack "OK\r\n") B.empty 
    closeSerial s
    putStrLn "Reacquiring data"
    getSingleRead x
    putStrLn "New average value" 
    calibrate [x]) [path,val]

-- | Stream function
stream :: [String] -> IO ()
stream (path:"0":[]) = do
	putStrLn "Done" 
stream (path:times:[]) = doIfFileExists path (\[x,y] -> do
	s <- openSerial x defaultSerialSettings { timeout = 10 }
	threadDelay 2000000
	putStrLn "Timestamp, RIU"
	send s $ B.pack "5"
	streamhelper s (read y::Int)
	send s $ B.pack "6"
	closeSerial s) [path,times]

streamhelper :: SerialPort -> Int -> IO ()
streamhelper s 0 = putStrLn "All done!"
streamhelper s times = do 
	acc <- recursiveReadUntil s (B.pack "OK\r\n") B.empty
	let frame = parse $ (lines . B.unpack) acc
	putStrLn $ (show (timestamp frame)) ++ ", " ++ (show (riu frame))
	streamhelper s (times-1)

-- | doIfFileExists function
doIfFileExists :: FilePath -> ([a] -> IO ()) -> [a] -> IO ()
doIfFileExists path function args = do
    exists <- doesFileExist path
    case exists of 
        False -> do
            putStrLn "No device found - looking in /dev/"
            findspr []
        True -> function args
        


-- Data structure to define a reading frame - consists of two records
-- a Time stamp and a list of voltage values

data ReadingFrame = ReadingFrame { timestamp :: Int
                                 , values :: [(Int,Double)]
                                 , avg :: Double
                                 , riu :: Double
                                 } deriving (Show)

parse :: [String] -> ReadingFrame
parse list = parse_single list (ReadingFrame { timestamp = 0, values = [], avg = 0, riu = 0})

parse_single :: [String] -> ReadingFrame -> ReadingFrame
parse_single [] acc = let volts = [v | (_,v) <- values acc] 
                          average = realToFrac (sum volts) / genericLength volts 
                          ri = rifind volts 3
                      in ReadingFrame { timestamp = timestamp acc
                                      , values = values acc
                                      , avg = average
                                      , riu = ri
                                      }
parse_single (x:xs) acc = 
    if ("T: " `isPrefixOf` x) 
        then let ts = read (dropWhile (/=' ') x) :: Int
             in parse_single xs $ ReadingFrame { timestamp = ts
                                               , values = (values acc)
                                               , avg = (avg acc)
                                               , riu = (riu acc)
                                               }
        else if (':' `elem` x) 
                then let [idx,volts] = splitOn ":" x 
                    in parse_single xs $ ReadingFrame { timestamp = timestamp acc
                                               , values = (values acc) ++ 
                                                    [(read idx :: Int, read volts :: Double)]
                                               , avg = avg acc
                                               , riu = riu acc
                                               }
                else parse_single xs acc
                
--------- Functions for curve fitting and minima calculation ----

-- | Function that returns an array of the difference between 
--   successive numbers. Produces offset of 1/2 pixel
diff :: [Double] -> [Double]
diff x = zipWith (-) (tail x) (init x)

-- | Determines the length of the array before a zero is crossed
--   Adds a 1/2 pixel offset 
zeroCross :: [Double] -> Int
zeroCross x = length $ takeWhile (<0) x

-- | moving average smoother
--   Produces a window/2 offset
movavg :: [Double] -> Int -> [Double] -> [Double]
movavg [] window acc = reverse acc 
movavg xs@(x:xt) window acc = if (length xs >= 5) 
                                    then let sample = take window xs
                                             avg = (sum sample) / (fromIntegral window)
                                         in movavg xt window (avg:acc)
                                    else movavg [] window acc

-- | moving average wrapper ----- 
movingaverage :: [Double] -> Int -> [Double]
-- produces offset of window/2
movingaverage x window = movavg x window [] 

-- | Finds the refractive index for a smoothed function based on pixel interpolation
rifind :: [Double] -> Int -> Double
rifind arr window = let (min, max) = (1.320, 1.368) -- from the Spreeta datasheet
                        pixel = (zeroCross . diff) $ movingaverage arr window
                        minima = (fromIntegral pixel) + 1 + fromIntegral(window)/2
                        -- the zeroCross and diff functions produce a 0.5 pixel offset 
                        -- each, while the window produces a window/2 offset. 
                    in min + ((max - min) * minima / 128)
