module Main where

import Data.List (isInfixOf)
import System.Directory (getDirectoryContents, removeFile)
import System.IO

main :: IO ()
main = do 
    putStr "What is your name?\nName: "
    name <- getLine
    if null name then main else putStrLn $ "Hi, " ++ name ++ "!"
    putStr "Substring: "
    pattern <- getLine
    if null pattern
      then putStrLn "Canceled" 
      else do
        allFiles <- getDirectoryContents "."
        let satisfactoryFiles = filter (isInfixOf pattern) allFiles
        mapM_ (\str -> putStrLn ("Removing file: " ++ str) >> removeFile str) satisfactoryFiles