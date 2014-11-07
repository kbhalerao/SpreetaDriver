-- spr.hs
-- Spreeta interface in Haskell FTW!

import System.Environment
import Data.List
import System.IO
import System.Directory
import Data.String.Utils
import qualified Data.ByteString.Char8 as B
import System.Hardware.Serialport

dispatch :: [(String, [String] -> IO ())]
dispatch = [ ("find", findspr) 
            ,("read", readspr)
            ,("calibrate", calibrate)
            ]

main = do
    (command:args) <- getArgs
    let (Just action) = lookup command dispatch
    action args


-- Helper functions
printfilter :: [FilePath] -> IO ()
printfilter dir = let filtered = filter (isPrefixOf "tty.usb") dir
                    in print filtered

-- Find USB devices function                      
findspr :: [FilePath] -> IO ()
findspr [] = findspr ["/dev/"]         
findspr [filepath] = do dir <- getDirectoryContents filepath
                        printfilter dir

-- Read a single reading
recursiveread :: SerialPort -> B.ByteString -> IO B.ByteString
recursiveread s acc = do recd <- recv s 10000
                         print recd
                         if (recd == B.empty)
                            then return acc
                            else recursiveread s $ B.append acc recd
                    


readspr :: [FilePath] -> IO ()
readspr [path] = do exists <- doesFileExist path 
                    case exists of False -> print "No such file"
                                   True -> do s <- openSerial path defaultSerialSettings
                                              send s $ B.pack "1"
                                              acc <- recursiveread s B.empty
                                              print "got acc"
                                              print acc

calibrate :: [String] -> IO ()
calibrate = undefined

