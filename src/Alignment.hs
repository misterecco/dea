module Alignment where

import Parser

import Debug.Trace


data EventDiff = EDLeft CodeEvent | EDCommon CodeEvent | EDRight CodeEvent
type TraceDiff = [EventDiff]

instance Show EventDiff where
    show (EDLeft (CE eventType loc _)) =
        unlines ["LEFT: ", "Event: " ++ show eventType, "Loc: " ++ show loc]
    show (EDCommon (CE eventType loc _)) =
        unlines ["               COMMON: ", "Event: " ++ show eventType, "Loc: " ++ show loc]
    show (EDRight (CE eventType loc _)) =
        unlines ["                              RIGHT: ", "Event: " ++ show eventType, "Loc: " ++ show loc]

alignTraces :: CallTrace -> CallTrace -> (Integer, TraceDiff)
alignTraces leftTrace rightTrace = 
    if null leftTrace || null rightTrace || (head leftTrace) /= (head rightTrace)
        then (-1, [])
        else align leftTrace rightTrace (0, [])
    where
        align [] [] (score, acc) = (score, reverse acc)
        align [] (event:rightTrace) (score, acc) = 
            align [] rightTrace (score, ((EDRight event):acc))
        align (event:leftTrace) [] (score, acc) = 
            align leftTrace [] (score, ((EDLeft event):acc))
        align (leftEv@(CE _ _ leftSt):leftTr) (rightEv@(CE _ _ rightSt):rightTr) (score, acc) =
            if leftEv == rightEv 
                then align leftTr rightTr (score + 1, ((EDCommon leftEv):acc))
            else if (length leftSt) <= (length rightSt)
                then align (leftEv:leftTr) rightTr (score, ((EDRight rightEv):acc))
            else align leftTr (rightEv:rightTr) (score, ((EDLeft leftEv):acc))
