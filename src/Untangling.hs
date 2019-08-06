{-# LANGUAGE Strict #-}

module Untangling where

import Data.List ( find, delete )
import qualified Data.Map.Strict as M
import Debug.Trace

import Parser
import Lean


untangleEvents :: [LeanCodeEvent] -> [LeanCallTrace]
untangleEvents events = untangle [] M.empty events (length events)
    where
        untangle results openTraces (event:events) left = do
            let (LCE _ _ st) = event
            let (ev, ot) = if left `mod` 1000 == 0
                then trace (show (M.size openTraces) ++ " " ++ show left) (event, openTraces)
                else (event, openTraces)
            -- let (ev, ot) = (event, openTraces)
            case findMatchingTrace ot ev of
                Just (matchingTrace, newOpenTraces) -> do
                    let newTrace = ev:matchingTrace
                    case ev of
                        (LCE FunctionExit _ []) -> untangle ((reverse newTrace):results) newOpenTraces events (left-1)
                        _ -> untangle results (M.insert st newTrace newOpenTraces) events (left-1)
                Nothing -> do
                    let newOpenTraces = trace ("Didn't find proper predecessor for: " ++ show ev) openTraces
                    untangle results newOpenTraces events (left-1)
        -- should not happen in complete trace
        untangle results openTraces [] _ = reverse ((M.elems openTraces) ++ results)
        findMatchingTrace openTraces event = case event of
            (LCE FunctionEnter _ [_]) -> Just ([], openTraces)
            (LCE FunctionEnter _ st) -> findMatchingOpenEvent (tail st) openTraces
            (LCE GeneratorEnter _ st) -> findMatchingOpenEvent (tail st) openTraces
            (LCE FunctionExit loc st) -> findMatchingOpenEvent (loc:st) openTraces
            (LCE GeneratorSuspend loc st) -> findMatchingOpenEvent (loc:st) openTraces
            (LCE GeneratorYield loc st) -> findMatchingOpenEvent st openTraces
            (LCE IfStmtThen _ st) -> findMatchingOpenEvent st openTraces
            (LCE IfStmtElse _ st) -> findMatchingOpenEvent st openTraces
        findMatchingOpenEvent st openTraces = case openTraces M.!? st of
            Nothing -> Nothing
            Just tr -> Just (tr, M.delete st openTraces)
