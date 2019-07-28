{-# LANGUAGE Strict #-}

module Untangling where

import Data.List ( find, delete )
import Debug.Trace

import Parser
import Lean


untangleEvents :: [LeanCodeEvent] -> [LeanCallTrace]
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
                        (LCE FunctionExit _ []) -> untangle ((reverse newTrace):results) newOpenTraces events (left-1)
                        _ -> untangle results (newTrace:newOpenTraces) events (left-1)
                Nothing -> do
                    let newOpenTraces = trace ("Didn't find proper predecessor for: " ++ show ev ++ " in " ++ (show $ map head openTraces)) openTraces
                    untangle results newOpenTraces events (left-1)
        -- should not happen in complete trace
        untangle results openTraces [] _ = reverse (openTraces ++ results)
        findMatchingTrace openTraces event = case event of
            (LCE FunctionEnter _ [_]) -> Just ([], openTraces)
            (LCE FunctionEnter _ st) -> findMatchingOpenEvent (tail st) openTraces
            (LCE GeneratorEnter _ st) -> findMatchingOpenEvent (tail st) openTraces
            (LCE FunctionExit loc st) -> findMatchingOpenEvent (loc:st) openTraces
            (LCE GeneratorSuspend loc st) -> findMatchingOpenEvent (loc:st) openTraces
            (LCE GeneratorYield loc st) -> findMatchingOpenEvent st openTraces
            (LCE IfStmtThen _ st) -> findMatchingOpenEvent st openTraces
            (LCE IfStmtElse _ st) -> findMatchingOpenEvent st openTraces
        findMatchingOpenEvent st [e] = Just (e, [])
        findMatchingOpenEvent st openTraces =
            case (find (isStackMatching st) openTraces) of
                Nothing -> Nothing
                Just e -> Just (e, delete e openTraces)
        isStackMatching st trace = case trace of
            [] -> False
            (LCE _ _ st1):_ -> st1 == st
