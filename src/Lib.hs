module Lib where

import Data.List.Split ( endBy )

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

data EventType = FunctionEnter | FunctionExit | IfStmtThen | IfStmtElse
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
        ("FUNCTION ENTER" : xs) -> readEvent FunctionEnter "FUNCTION ENTER END" xs
        ("FUNCTION EXIT" : xs) -> do
                let (locs, rest) = span (/= "FUNCTION EXIT END") xs
                let n = length locs
                let st = map read $ take (n-1) locs
                [(CE FunctionExit (head st) (tail st), (unlines $ tail rest))]
        ("IF STMT - THEN" : xs) -> readEvent IfStmtThen "IF STMT - THEN END" xs
        ("IF STMT - ELSE" : xs) -> readEvent IfStmtElse "IF STMT - ELSE END" xs
        _ : _ -> []
        where
            readEvent eventType endMarker xs = do
                let (locs, rest) = span (/= endMarker) xs
                let st = map read locs
                [(CE eventType (head st) (tail st), (unlines $ tail rest))]
    readList input = do
        let entries = endBy "--\n" input
        let locs = map read entries
        [(locs, "")]

instance Show CodeEvent where
    show (CE eventType loc st) =
        "Event: " ++ show eventType ++ "\nLoc: " 
            ++ show loc ++ "\nStack:\n" ++ (unlines $ map show st)
