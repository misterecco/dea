module Main where

import Control.Monad
import Data.List ( isSuffixOf )
import Data.Map ( (!) )
import Data.Attoparsec.ByteString.Char8
import System.IO
import System.Environment ( getArgs )
import System.Exit ( exitFailure, exitSuccess )
import System.Process

import qualified Data.ByteString as B

import Alignment
import Parser


runFile :: FilePath -> IO [CallTrace]
runFile f = do
    fileContent <- B.readFile f
    -- let events = B.readFile f >>= parseOnly callTraceParser
    case (parseOnly callTraceParser fileContent) of
        Left err -> do
            fail $ show err
        Right events -> do
            -- mapM_ print events
            putStrLn $ "Number of events: " ++ show (length events)
            putStrLn "=============================="
            let traces = untangleEvents events
            -- mapM_ putStrLn (formatTraces traces)
            return traces


getTrace :: [FilePath] -> IO [CallTrace]
getTrace fs = mapM runFile fs >>= (return . commonElements)


runFiles :: [FilePath] -> IO ()
runFiles fs = do
    let n = length fs
    if (n `mod` 2) > 0
        then do
            fail "Please provide even number of traces"
        else do
            let (fsA, fsB) = splitAt (n `div` 2) fs
            traceA <- getTrace fsA
            traceB <- getTrace fsB
            diffTraces traceA traceB


commonElements :: Eq a => [[a]] -> [a]
commonElements [] = []
commonElements (h:t) = ct h t
    where
        ct h t = filter (\tr -> all (elem tr) t) h


diffTraces :: [CallTrace] -> [CallTrace] -> IO ()
diffTraces tracesA tracesB = do
    let (matched, unmatchedLeft, unmatchedRight) = if (length tracesA) <= (length tracesB)
        then analyzeTraces tracesA tracesB
        else let (m, ul, ur) = analyzeTraces tracesB tracesA in (map flipDiff m, ur, ul)
    let interestingMatched = filterNodiffs matched
    -- putStrLn "=============== Aligned ==============="
    -- mapM_ (putStrLn . show) matched
    putStrLn "=============== Aligned - filtered ==============="
    putStrLn $ "Number of all traces: " ++ show (length matched)
    putStrLn $ "Number of diffing traces: " ++ show (length interestingMatched)
    mapM_ (putStrLn . show) interestingMatched
    -- putStrLn "=============== Unmatched left ==============="
    -- mapM_ (putStrLn . show) (formatTraces unmatchedLeft)
    -- putStrLn "=============== Unmatched right ==============="
    -- mapM_ (putStrLn . show) (formatTraces unmatchedRight)
    putStrLn "========================================"


printUsage :: IO ()
printUsage =
    mapM_ putStrLn [ "dea <path_to_input_file>" ]


main :: IO ()
main = do
    args <- getArgs
    case args of
        ["--help"] -> printUsage
        fs@[_, _] -> runFiles fs
        fs@[_, _, _, _] -> runFiles fs
        fs@[_, _, _, _, _, _] -> runFiles fs
        _ -> printUsage
