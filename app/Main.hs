module Main where

import Control.Monad
import Data.List ( isSuffixOf )
import Data.Map ( (!) )
import System.IO
import System.Environment ( getArgs )
import System.Exit ( exitFailure, exitSuccess )
import System.Process

import Lib

runFile :: FilePath -> IO ()
runFile f = do
    s <- readFile f
    let events = read s :: [CodeEvent]
    mapM_ print events


printUsage :: IO ()
printUsage =
    mapM_ putStrLn [ "dea <path_to_input_file>" ]


main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--help"] -> printUsage
    [fs] -> runFile fs
    _ -> printUsage
