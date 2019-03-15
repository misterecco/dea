module Lib where

import Data.List (isInfixOf)
import Data.List.Split ( endBy )

import Debug.Trace

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Loc = CL {
    functionName :: String,
    filePath :: String,
    line :: Integer,
    column :: Integer
}
    deriving (Eq)

type Stack = [Loc]

data EventType = FunctionEnter | FunctionExit | GeneratorEnter | GeneratorYield | IfStmtThen | IfStmtElse
    deriving (Show, Eq, Ord)

data CodeEvent = CE EventType Loc Stack
    deriving (Eq)


instance Show Loc where
    show (CL fun path line col) = 
        fun ++ " at " ++ path ++ ":" ++ show line ++ "," ++ show col

-- instance Eq Loc where
--     (==) (CL fun1 path1 line1 col1) (CL fun2 path2 line2 col2) = 
--         fun1 == fun2 && path1 == path2 && line1 == line2 && col1 == col2

        
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
        ("IF STMT - THEN" : xs) -> readEvent IfStmtThen 0 xs
        ("IF STMT - ELSE" : xs) -> readEvent IfStmtElse 0 xs
        _ : _ -> []
        where
            readEvent eventType m xs = do
                let locs = takeWhile (/= "=") xs
                let n = length locs
                let st = map read $ filter (\x -> not (isInfixOf "<JSGenerator>" x)) $ take (n-m) locs
                [(CE eventType (head st) (tail st), [])]
    readList input = do
        let entries = endBy "--\n" input
        let locs = map read entries
        [(locs, "")]

instance Show CodeEvent where
    show (CE eventType loc st) =
        "Event: " ++ show eventType ++ "\nLoc: " 
            ++ show loc ++ "\nStack:\n" ++ (unlines $ map show st)
