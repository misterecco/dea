{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Control.Applicative
import Control.Monad.Writer
import Data.Attoparsec.ByteString.Char8
import Data.Hashable
import Data.List ( isInfixOf, find, delete )
import Debug.Trace
import GHC.Generics (Generic)

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C


data Loc = CL {
    functionName :: B.ByteString,
    filePath :: B.ByteString,
    line :: Integer,
    column :: Integer
} deriving (Eq, Generic, Hashable, Ord)

instance Show Loc where
    show (CL fun path line col) =
        show fun ++ " at " ++ show path ++ ":" ++ show line ++ "," ++ show col

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
    l <- takeTill (== '\n')
    case parseLoc l of
        Just loc -> return loc
        Nothing -> fail $ "Could not parse loc: " ++ show l

parseLoc :: B.ByteString -> Maybe Loc
parseLoc l = do
    let (name, (_:rest)) = span (/= "at") (C.words l)
    let (path, (_:loc:_)) = span (/= "@@") rest
    (line, restLoc) <- C.readInteger loc
    (col, _) <- C.readInteger (C.tail restLoc)
    return $ CL (C.unwords name) (C.unwords path) line col

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
        skipWhile (/= '\n')
        endOfLine
    string "="
    endOfLine
    string "--"
    let st = filterStack locs
    let hashedSt = map hashed (newStack eventType st)
    return $ CE eventType (head st) hashedSt
        where
            filterStack = filter (\loc -> line loc >= 0)
            newStack eventType stack =
                if elem eventType [FunctionEnter, GeneratorEnter]
                    then stack
                    else tail stack

callTraceParser :: Parser CallTrace
callTraceParser = many $ codeEventParser <* endOfLine


untangleEvents :: [CodeEvent] -> [CallTrace]
untangleEvents events = untangle [] [] events (length events)
    where
        untangle results [] [] _ = reverse results
        untangle results openTraces (event:events) left = do
            -- let ev = traceShowId event
            let (ev, ot) = trace (show (length openTraces) ++ " " ++ show left) (event, openTraces)
            let (matchingTrace, newOpenTraces) = findMatchingTrace ot ev
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

