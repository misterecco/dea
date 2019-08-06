{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}

module Parser where

import Control.Applicative
import Control.DeepSeq
import Control.Monad.Writer
import Data.Attoparsec.ByteString
import Data.Attoparsec.ByteString.Char8 (decimal, char, endOfLine, space)
import Data.ByteString.Short (toShort, ShortByteString)
import Debug.Trace
import GHC.Generics (Generic)

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Short as SB

data Loc = CL {
    functionName :: !ShortByteString,
    filePath :: !ShortByteString,
    line :: !Int,
    column :: !Int
} deriving (Eq, Generic, Ord, NFData)

instance Show Loc where
    show (CL fun path line col) =
        show fun ++ " at " ++ show path ++ ":" ++ show line ++ "," ++ show col

type Stack = [Loc]

data EventType
    = FunctionEnter
    | FunctionExit
    | GeneratorEnter
    | GeneratorYield
    | GeneratorSuspend
    | IfStmtThen
    | IfStmtElse
    deriving (Show, Eq, Ord, Generic, NFData)

data CodeEvent = CE EventType !Loc !Stack
    deriving (Eq, Ord, Generic, NFData)

instance Show CodeEvent where
    show (CE eventType loc st) =
        "Event: " ++ show eventType ++ "\nLoc: "
            ++ show loc ++ "\nStack:\n" ++ (unlines $ map show st)

type CallTrace = [CodeEvent]


locParser :: Parser Loc
locParser = do
    skipMany space
    decimal
    char ':'
    l <- takeTill (== 10)
    case parseLoc l of
        Just loc -> return loc
        Nothing -> fail $ "Could not parse loc: " ++ show l

parseLoc :: B.ByteString -> Maybe Loc
parseLoc l = do
    -- let l = traceShowId lll
    let (name, (_:rest)) = span (/= "at") (C.words l)
    let (path, (_:loc:_)) = span (/= "@@") rest
    (line, restLoc) <- C.readInt loc
    (col, _) <- C.readInt (C.tail restLoc)
    return $! CL (toShort $! C.unwords name) (toShort $! C.unwords path) line col

eventTypeParser :: Parser EventType
eventTypeParser =
        (string "FUNCTION ENTER" >> return FunctionEnter)
    <|> (string "FUNCTION EXIT" >> return FunctionExit)
    <|> (string "GENERATOR ENTER" >> return GeneratorEnter)
    <|> (string "GENERATOR YIELD" >> return GeneratorYield)
    <|> (string "GENERATOR SUSPEND" >> return GeneratorSuspend)
    <|> (string "IF STMT - THEN" >> return IfStmtThen)
    <|> (string "IF STMT - ELSE" >> return IfStmtElse)

codeEventParser :: Parser CodeEvent
codeEventParser = do
    eventType <- eventTypeParser <* endOfLine
    locs <- many (locParser <* endOfLine)
    when (elem eventType [FunctionExit, GeneratorYield]) $ do
        _ <- (string "->")
        skipWhile (/= 10)
        endOfLine
    string "="
    endOfLine
    string "--"
    let st = filterStack locs
    return $ CE eventType (head st) (newStack eventType st)
        where
            filterStack = filter (\loc -> line loc >= 0)
            newStack eventType stack =
                if elem eventType [FunctionEnter, GeneratorEnter]
                    then stack
                    else tail stack

callTraceParser :: Parser CallTrace
callTraceParser = many' $ codeEventParser <* endOfLine

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
