module Parser where

import Control.Monad.Writer
import Data.List ( isInfixOf, find, delete )
import Data.List.Split ( endBy )

data Loc = CL {
    functionName :: String,
    filePath :: String,
    line :: Integer,
    column :: Integer
} deriving (Eq, Ord)

type Stack = [Loc]

data EventType
    = FunctionEnter
    | FunctionExit
    | GeneratorEnter
    | GeneratorYield
    | GeneratorSuspend
    | IfStmtThen
    | IfStmtElse
    deriving (Show, Eq, Ord)

data CodeEvent = CE EventType Loc Stack
    deriving (Eq, Ord)

type CallTrace = [CodeEvent]

instance Show Loc where
    show (CL fun path line col) =
        fun ++ " at " ++ path ++ ":" ++ show line ++ "," ++ show col

instance Read Loc where
    readsPrec _ input = case (lines input) of
        [] -> []
        (l:ls) -> do
            let w = drop 1 $ words l
            let fname = head w
            let w2 = drop 2 w
            let loc = head w2
            let (path, pos) = span (/= ':') loc
            let line = read $ takeWhile (/= ',') $ tail pos
            let col = read $ takeWhile (/= '(') $ tail $ dropWhile (/= ',') pos
            [(CL fname path line col, unlines ls)]


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
                [(CE eventType (head st) (newStack eventType st), [])]
            newStack eventType stack =
                if elem eventType [FunctionEnter, GeneratorEnter]
                    then stack else tail stack
    readList input = do
        let entries = endBy "--\n" input
        let locs = map read entries
        [(locs, "")]

instance Show CodeEvent where
    show (CE eventType loc st) =
        "Event: " ++ show eventType ++ "\nLoc: "
            ++ show loc ++ "\nStack:\n" ++ (unlines $ map show st)


untangleEvents :: [CodeEvent] -> [CallTrace]
untangleEvents events = untangle [] [] events
    where
        untangle results [] [] = reverse results
        untangle results openTraces (event:events) = do
            let (matchingTrace, newOpenTraces) = findMatchingTrace openTraces event
            let newTrace = event:matchingTrace
            case event of
                (CE FunctionExit _ []) -> untangle ((reverse newTrace):results) newOpenTraces events
                _ -> untangle results (newTrace:newOpenTraces) events
        -- should not happen in complete trace
        untangle results openTraces [] = reverse (openTraces ++ results)
        findMatchingTrace openTraces event = case event of
            (CE FunctionEnter _ [_]) -> ([], openTraces)
            (CE FunctionEnter _ st) -> findMatchingOpenEvent (tail st) openTraces
            (CE GeneratorEnter _ st) -> findMatchingOpenEvent (tail st) openTraces
            (CE FunctionExit loc st) -> findMatchingOpenEvent (loc:st) openTraces
            (CE GeneratorSuspend loc st) -> findMatchingOpenEvent (loc:st) openTraces
            (CE GeneratorYield loc st) -> findMatchingOpenEvent st openTraces
            (CE IfStmtThen _ st) -> findMatchingOpenEvent st openTraces
            (CE IfStmtElse _ st) -> findMatchingOpenEvent st openTraces
        findMatchingOpenEvent st openTraces =
            case (find (isStackMatching st) openTraces) of
                Nothing -> error $ "Didn't find proper predecessor for: " ++ show st ++ " in: " ++ show openTraces
                Just e -> (e, delete e openTraces)
        isStackMatching st trace = case trace of
            [] -> False
            (CE _ _ st1):_ -> st1 == st

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

