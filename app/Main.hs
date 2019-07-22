{-# LANGUAGE Strict #-}

module Main where

import Control.DeepSeq
import Control.Monad
import Data.Attoparsec.ByteString
import Data.List ( isSuffixOf )
import Data.Map ( (!) )
import Data.IORef
import System.IO
import System.Environment ( getArgs )
import System.Exit ( exitFailure, exitSuccess )
import System.Process

import qualified Data.ByteString.Char8 as B

import Alignment
import Parser


runFile :: FilePath -> IO [CallTrace]
runFile f = do
    res <- withFile f ReadMode $ \h -> do
        hSetBinaryMode h True
        fileContent <- B.hGetContents h
        return $! parseOnly callTraceParser fileContent
    case res of
        Left err -> fail err
        -- Fail remainder context err -> do
        --     putStrLn "REMAINDER:"
        --     B.putStrLn remainder
        --     putStrLn "CONTEXT:"
        --     mapM_ putStrLn context
        --     putStrLn "ERROR:"
        --     putStrLn err
        --     fail err
        Right events -> do
        -- Done _ events -> do
            -- mapM_ print events
            putStrLn $ "Number of events: " ++ show (length events)
            putStrLn "=============================="
            return $! untangleEvents events
            -- mapM_ putStrLn (formatTraces traces)


getTrace :: [FilePath] -> IO [CallTrace]
getTrace fs = do
    firstTrace <- runFile (head fs)
    let _ = rnf firstTrace
    result <- newIORef firstTrace
    forM (tail fs) $ \f -> do
        nextTrace <- runFile f
        currentResult <- readIORef result
        let nextResult = commonElements nextTrace currentResult
        let _ = rnf nextResult
        writeIORef result nextResult
    readIORef result


runFiles :: [FilePath] -> IO ()
runFiles fs = do
    let n = length fs
    if (n `mod` 2) > 0
        then fail "Please provide even number of traces"
        else do
            let (fsA, fsB) = splitAt (n `div` 2) fs
            traceA <- getTrace fsA
            putStrLn "==========================================="
            traceB <- getTrace fsB
            diffTraces traceA traceB


commonElements :: Eq a => [a] -> [a] -> [a]
commonElements l = filter (`elem` l)


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
