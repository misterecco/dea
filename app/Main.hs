module Main where

import Control.Monad
import Data.List ( isSuffixOf )
import Data.Map ( (!) )
import System.IO
import System.Environment ( getArgs )
import System.Exit ( exitFailure, exitSuccess )
import System.Process

import Alignment
import Parser

runFile :: FilePath -> IO [CallTrace]
runFile f = do
    s <- readFile f
    let events = read s :: [CodeEvent]
    -- mapM_ print events
    putStrLn "=============================="
    let traces = untangleEvents events
    mapM_ putStrLn (formatTraces traces)
    return traces


runFiles :: [FilePath] -> IO ()
runFiles fs = do
    tracesA <- runFile $ fs !! 0
    tracesB <- runFile $ fs !! 1
    let (matched, unmatchedLeft, unmatchedRight) = if (length tracesA) <= (length tracesB)
        then analyzeTraces tracesA tracesB
        else let (m, ul, ur) = analyzeTraces tracesB tracesA in (map flipDiff m, ur, ul)
    putStrLn "=============== Aligned ==============="
    mapM_ (putStrLn . show) matched
    putStrLn "=============== Unmatched left ==============="
    mapM_ (putStrLn . show) unmatchedLeft
    putStrLn "=============== Unmatched right ==============="
    mapM_ (putStrLn . show) unmatchedRight


printUsage :: IO ()
printUsage =
    mapM_ putStrLn [ "dea <path_to_input_file>" ]


main :: IO ()
main = do
    args <- getArgs
    case args of
        ["--help"] -> printUsage
        fs@[_, _] -> runFiles fs
        _ -> printUsage
