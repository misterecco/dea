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
import Data.Hashable
import Data.List ( isInfixOf, find, delete )
import Debug.Trace
import GHC.Generics (Generic)

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Short as SB

data Loc = CL {
    functionName :: !SB.ShortByteString,
    filePath :: !SB.ShortByteString,
    line :: !Int,
    column :: !Int
} deriving (Eq, Generic, Hashable, Ord, NFData)

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
    let (name, (_:rest)) = span (/= "at") (C.words l)
    let (path, (_:loc:_)) = span (/= "@@") rest
    (line, restLoc) <- C.readInt loc
    (col, _) <- C.readInt (C.tail restLoc)
    return $! CL (SB.toShort $! C.unwords name) (SB.toShort $! C.unwords path) line col

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


untangleEvents :: [CodeEvent] -> [CallTrace]
untangleEvents events = untangle [] [] events (length events)
    where
        untangle results [] [] _ = reverse results
        untangle results openTraces (event:events) left = do
            -- let (ev, ot) = trace (show (length openTraces) ++ " " ++ show left) (event, openTraces)
            let (ev, ot) = (event, openTraces)
            case findMatchingTrace ot ev of
                Just (matchingTrace, newOpenTraces) -> do
                    let newTrace = ev:matchingTrace
                    case ev of
                        (CE FunctionExit _ []) -> untangle ((reverse newTrace):results) newOpenTraces events (left-1)
                        _ -> untangle results (newTrace:newOpenTraces) events (left-1)
                Nothing -> do
                    let newOpenTraces = trace ("Didn't find proper predecessor for: " ++ show ev ++ " in " ++ (show $ map head openTraces)) openTraces
                    untangle results newOpenTraces events (left-1)
        -- should not happen in complete trace
        untangle results openTraces [] _ = reverse (openTraces ++ results)
        findMatchingTrace openTraces event = case event of
            (CE FunctionEnter _ [_]) -> Just ([], openTraces)
            (CE FunctionEnter _ st) -> findMatchingOpenEvent (tail st) openTraces
            (CE GeneratorEnter _ st) -> findMatchingOpenEvent (tail st) openTraces
            (CE FunctionExit loc st) -> findMatchingOpenEvent (loc:st) openTraces
            (CE GeneratorSuspend loc st) -> findMatchingOpenEvent (loc:st) openTraces
            (CE GeneratorYield loc st) -> findMatchingOpenEvent st openTraces
            (CE IfStmtThen _ st) -> findMatchingOpenEvent st openTraces
            (CE IfStmtElse _ st) -> findMatchingOpenEvent st openTraces
        findMatchingOpenEvent st [e] = Just (e, [])
        findMatchingOpenEvent st openTraces =
            case (find (isStackMatching st) openTraces) of
                Nothing -> Nothing
                Just e -> Just (e, delete e openTraces)
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

removeStack :: CodeEvent -> CodeEvent
removeStack (CE t l _) = CE t l []

removeStacks :: CallTrace -> CallTrace
removeStacks = map removeStack

formatTraces :: [CallTrace] -> [String]
formatTraces traces = execWriter (showTraces traces)

