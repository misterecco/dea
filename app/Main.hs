{-# LANGUAGE Strict #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.DeepSeq
import Control.Monad
import Data.Attoparsec.ByteString
import Data.Attoparsec.ByteString.Char8 (endOfLine)
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

chunkSize :: Int
chunkSize = 1024

runFile :: FilePath -> IO [CallTrace]
runFile f = do
    h <- openFile f ReadMode
    hSetBinaryMode h True
    firstChunk <- B.hGet h chunkSize
    parseUntilDone [] h $ parseEvent firstChunk
      where
        parseEvent = parse (codeEventParser <* endOfLine)
        parseUntilDone acc h = \case
            Fail remainder context err -> do
                putStrLn "REMAINDER:"
                B.putStrLn remainder
                putStrLn "CONTEXT:"
                mapM_ putStrLn context
                putStrLn "ERROR:"
                putStrLn err
                fail err
            Done i event -> do
                let _ = rnf event
                if i == B.empty
                    then do
                        nextChunk <- B.hGet h chunkSize
                        if nextChunk == B.empty
                            then do
                                let eventList = event:acc
                                putStrLn "=============================="
                                putStrLn $ "Number of events: " ++ show (length eventList)
                                return $ untangleEvents (reverse eventList)
                            else parseUntilDone (event:acc) h $ parseEvent nextChunk
                    else parseUntilDone (event:acc) h $ parseEvent i
            Partial (cont) -> do
                nextChunk <- B.hGet h chunkSize
                parseUntilDone acc h $ cont nextChunk


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
