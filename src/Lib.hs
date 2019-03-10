module Lib where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Loc = CL {
    functionName :: String,
    filePath :: String,
    line :: Integer,
    column :: Integer
}

instance Show Loc where
    show (CL fun path line col) = 
        fun ++ " at " ++ path ++ ":" ++ show line ++ "," ++ show col

instance Eq Loc where
    (==) (CL fun1 path1 line1 col1) (CL fun2 path2 line2 col2) = 
        fun1 == fun2 && path1 == path2 && line1 == line2 && col1 == col2

-- TODO(tkepa)
instance Read Loc where
    readsPrec _ input = do
        let ls = lines input
        case ls of
            [] -> []
            ("FUNCTION ENTER" : xs) -> [(CL "fun" "path" 0 0, unlines xs)]
            _ : _ -> []


type Stack = [Loc]

data EventType = FunctionEnter | FunctionLeave | IfStmtThen | IfStmtElse
    deriving (Show, Eq, Ord)

data CodeEvent = CE EventType Loc Stack
    deriving (Show, Eq)
