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
    let printfilter dir = print $ filter (isPrefixOf "cu.") dir
    in getDirectoryContents filepath >>= printfilter

-- Read a single reading from the serial port
recursiveread :: SerialPort -> B.ByteString -> IO B.ByteString
recursiveread s acc = do 
    --print acc
    recd <- recv s 1000
    print recd
    if ((B.pack "Done\r\n") `B.isSuffixOf` recd)
        then recursiveread s $ B.append acc recd
        else return acc

-- Read a single reading from the serial port
getSingleRead :: FilePath -> IO [String]
getSingleRead path = let 
    getData s = do 
        send s $ B.pack "1"
        --acc <- recv s 1000
        --recv s 1000 >>= print
        --acc <- recv s 1000
        acc <- recursiveread s B.empty
        return $ (lines . B.unpack) acc
    in withSerial path defaultSerialSettings getData


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
        then let ts = read (dropWhile (/=' ') "T: 1234") :: Int
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

                            