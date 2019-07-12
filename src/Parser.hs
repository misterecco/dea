{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Parser where

import Control.Monad.Writer
-- import Control.DeepSeq
import Data.Hashable
import GHC.Generics (Generic)
import Data.List ( isInfixOf, find, delete )
import Data.List.Split ( endBy )
import Debug.Trace

data Loc = CL {
    functionName :: String,
    filePath :: String,
    line :: Integer,
    column :: Integer
} deriving (Eq, Generic, Hashable, Ord)

type Stack = [Hashed Loc]

data EventType
    = FunctionEnter
    | FunctionExit
    | GeneratorEnter
    | GeneratorYield
    | GeneratorSuspend
    | IfStmtThen
    | IfStmtElse
    deriving (Show, Eq, Ord)

data CodeEvent = CE EventType !Loc !Stack
    deriving (Eq, Ord)

type CallTrace = [CodeEvent]

instance Show Loc where
    show (CL fun path line col) =
        fun ++ " at " ++ path ++ ":" ++ show line ++ "," ++ show col

instance Read Loc where
    readsPrec _ input = case (lines input) of
        [] -> []
        (l:ls) -> do
            let tokens = words (traceShowId l)
            let (fname, path, line, col) = parseLoc tokens
            [(CL fname path (read line) (read col), unlines ls)]

parseLoc :: [String] -> (String, String, String, String)
parseLoc l = (unwords name, unwords path, line, col)
    where
        ((_:name), (_:rest)) = span (/= "at") l
        (path, (_:loc:_)) = span (/= "@@") rest
        (line, (_:col)) = span (/= ',') loc

instance Read CodeEvent where
    readsPrec _ input = case (lines input) of
        [] -> []
        ("FUNCTION ENTER" : xs) -> readEvent FunctionEnter 0 xs
        ("FUNCTION EXIT" : xs) -> readEvent FunctionExit 1 xs
        ("GENERATOR ENTER" : xs) -> readEvent GeneratorEnter 0 xs
        ("GENERATOR YIELD" : xs) -> readEvent GeneratorYield 1 xs
        ("GENERATOR SUSPEND" : xs) -> readEvent GeneratorSuspend 0 xs
        ("IF STMT - THEN" : xs) -> readEvent IfStmtThen 0 xs
        ("IF STMT - ELSE" : xs) -> readEvent IfStmtElse 0 xs
        _ : _ -> []
        where
            readEvent eventType m xs = do
                let locs = takeWhile (/= "=") xs
                let n = length locs
                let st = map read $ filter (\x -> not (isInfixOf "<JSGenerator>" x)) $ take (n-m) locs
                let filSt = filterStack st
                let hashedSt = map hashed (newStack eventType filSt)
                [(CE eventType (head filSt) hashedSt, [])]
            newStack eventType stack =
                if elem eventType [FunctionEnter, GeneratorEnter]
                    then stack else tail stack
            filterStack = filter (\loc -> line loc >= 0)
    readList input = do
        let entries = endBy "--\n" input
        let locs = map read entries
        [(locs, "")]

instance Show CodeEvent where
    show (CE eventType loc st) =
        "Event: " ++ show eventType ++ "\nLoc: "
            ++ show loc ++ "\nStack:\n" ++ (unlines $ map show st)


untangleEvents :: [CodeEvent] -> [CallTrace]
untangleEvents events = untangle [] [] events (length events)
    where
        untangle results [] [] _ = reverse results
        untangle results openTraces (event:events) left = do
            -- let ev = traceShowId event
            let (ev, ot) = trace (show (length openTraces) ++ " " ++ show left) (event, openTraces)
            let (matchingTrace, !newOpenTraces) = findMatchingTrace ot ev
            let newTrace = ev:matchingTrace
            case ev of
                (CE FunctionExit _ []) -> untangle ((reverse newTrace):results) newOpenTraces events (left-1)
                _ -> untangle results (newTrace:newOpenTraces) events (left-1)
        -- should not happen in complete trace
        untangle results openTraces [] _ = reverse (openTraces ++ results)
        findMatchingTrace openTraces event = case event of
            (CE FunctionEnter _ [_]) -> ([], openTraces)
            (CE FunctionEnter _ st) -> findMatchingOpenEvent (tail st) openTraces
            (CE GeneratorEnter _ st) -> findMatchingOpenEvent (tail st) openTraces
            (CE FunctionExit loc st) -> findMatchingOpenEvent ((hashed loc):st) openTraces
            (CE GeneratorSuspend loc st) -> findMatchingOpenEvent ((hashed loc):st) openTraces
            (CE GeneratorYield loc st) -> findMatchingOpenEvent st openTraces
            (CE IfStmtThen _ st) -> findMatchingOpenEvent st openTraces
            (CE IfStmtElse _ st) -> findMatchingOpenEvent st openTraces
        findMatchingOpenEvent st [e] = (e, [])
        findMatchingOpenEvent st openTraces =
            case (find (isStackMatching st) openTraces) of
                Nothing -> error $ "Didn't find proper predecessor for: " ++ show st ++ " in: " ++ show openTraces
                Just e -> (e, delete e openTraces)
        isStackMatching st trace = case trace of
            [] -> False
            (CE _ _ st1):_ -> stackEqual st1 st

stackEqual :: Stack -> Stack -> Bool
stackEqual lhs rhs = se lhs rhs 3 && (length lhs == length rhs)
  where
    se (l:ls) (r:rs) n = (hash l == hash r) && (se ls rs (n-1))
    se [] [] _ = True
    se _ _ 0 = True
    se _ _ _ = False

showEvent :: CodeEvent -> Writer [String] ()
showEvent (CE eventType loc st) = do
    tell [ "Event: " ++ show eventType ++ ", Loc: " ++ show loc ]

showTrace :: CallTrace -> Writer [String] ()
showTrace events = do
    mapM_ showEvent events
    tell [ "" ]

showTraces :: [CallTrace] -> Writer [String] ()
showTraces = mapM_ showTrace

formatTraces :: [CallTrace] -> [String]
formatTraces traces = execWriter (showTraces traces)

