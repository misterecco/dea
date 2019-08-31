{-# LANGUAGE Strict #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.DeepSeq
import Control.Monad
import Data.Attoparsec.ByteString
import Data.Attoparsec.ByteString.Char8 (endOfLine)
import Data.List ( isSuffixOf )
import Data.IORef
import System.IO
import System.Environment ( getArgs )
import System.Exit ( exitFailure, exitSuccess )
import System.Process

import qualified Data.ByteString.Char8 as B

import Alignment
import Lean
import Parser
import Untangling

chunkSize :: Int
chunkSize = 1024

runFile :: IORef StringsMap -> FilePath -> IO [LeanCallTrace]
runFile str f = do
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
            Done i ev -> do
                event <- toLeanCodeEvent str ev
                let _ = rnf event
                if i == B.empty
                    then do
                        nextChunk <- B.hGet h chunkSize
                        if nextChunk == B.empty
                            then do
                                let eventList = event:acc
                                putStrLn "=============================="
                                putStrLn $ "Number of events: " ++ show (length eventList)
                                hFlush stdout
                                return $ untangleEvents str (reverse eventList)
                            else parseUntilDone (event:acc) h $ parseEvent nextChunk
                    else parseUntilDone (event:acc) h $ parseEvent i
            Partial (cont) -> do
                nextChunk <- B.hGet h chunkSize
                parseUntilDone acc h $ cont nextChunk


getTrace :: IORef StringsMap -> [FilePath] -> IO [LeanCallTrace]
getTrace str fs = do
    firstTrace <- runFile str (head fs)
    let _ = rnf firstTrace
    result <- newIORef firstTrace
    forM (tail fs) $ \f -> do
        nextTrace <- runFile str f
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
            str <- newIORef $ emptyStringsMap
            traceA <- getTrace str fsA
            putStrLn "==========================================="
            traceB <- getTrace str fsB
            size <- getSize str
            putStrLn $ "Size of strings map: " ++ show size
            putStrLn $ "Number of left traces: " ++ show (length traceA)
            putStrLn $ "Number of right traces: " ++ show (length traceB)
            diffTraces str traceA traceB


commonElements :: Eq a => [a] -> [a] -> [a]
commonElements l = filter (`elem` l)


diffTraces :: IORef StringsMap -> [LeanCallTrace] -> [LeanCallTrace] -> IO ()
diffTraces str tracesA tracesB = do
    let (matched, unmatchedLeft, unmatchedRight) = if (length tracesA) <= (length tracesB)
        then analyzeTraces tracesA tracesB
        else let (m, ul, ur) = analyzeTraces tracesB tracesA in (map flipDiff m, ur, ul)
    let interestingMatched = filterNodiffs matched
    -- putStrLn "=============== Aligned ==============="
    -- mapM_ (putStrLn . show) matched
    putStrLn "=============== Aligned - filtered ==============="
    putStrLn $ "Number of all matched traces: " ++ show (length matched)
    putStrLn $ "Number of diffing traces: " ++ show (length interestingMatched)
    putStrLn $ "Number of unmatched left traces: " ++ show (length unmatchedLeft)
    putStrLn $ "Number of unmatched right traces: " ++ show (length unmatchedRight)
    mapM_ (printTraceDiff str) interestingMatched
    putStrLn "=============== Unmatched left ==============="
    mapM_ (printCallTrace str) unmatchedLeft
    putStrLn "=============== Unmatched right ==============="
    mapM_ (printCallTrace str) unmatchedRight
    putStrLn "========================================"


printUsage :: IO ()
printUsage =
    mapM_ putStrLn [ "dea-exe <positive traces> <negative traces>"
      , "    the number of the positive and the negative traces must be the same"
      , "    and cannot exceed 3"
      ]


main :: IO ()
main = do
    args <- getArgs
    case args of
        ["--help"] -> printUsage
        fs@[_, _] -> runFiles fs
        fs@[_, _, _, _] -> runFiles fs
        fs@[_, _, _, _, _, _] -> runFiles fs
        _ -> printUsage
