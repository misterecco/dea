module Alignment where

import Control.Monad.State
import Data.Map as M ( Map, empty, insertWith )
import Data.List ( nub, (\\), sort )

import Parser

import Debug.Trace


data EventDiff = EDLeft CodeEvent | EDCommon CodeEvent | EDRight CodeEvent
    deriving (Eq, Ord)
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


removeDuplicateTraces :: [CallTrace] -> [CallTrace] -> ([CallTrace], [CallTrace])
removeDuplicateTraces leftTraces rightTraces = do
    let leftUnique = nub leftTraces
    let rightUnique = nub rightTraces
    (leftUnique \\ rightUnique, rightUnique \\ leftUnique)


data SMPState = SMP {
    freeProposers :: Map CallTrace [(Integer, CallTrace, TraceDiff)],
    engagedProposers :: Map CallTrace [(Integer, CallTrace, TraceDiff)],
    acceptors :: Map CallTrace [(Integer, CallTrace, TraceDiff)]
}

type SMPMonad = State SMPState

initialSMPState :: SMPState
initialSMPState = SMP empty empty empty

addPreference :: CallTrace -> CallTrace -> SMPMonad ()
addPreference leftTrace rightTrace = do
    let (score, diff) = alignTraces leftTrace rightTrace
    st <- get
    let newFP = insertWith appendAndSort leftTrace [(score, rightTrace, diff)] (freeProposers st)
    let newAcc = insertWith appendAndSort rightTrace [(score, leftTrace, diff)] (acceptors st)
    put $ st { freeProposers = newFP, acceptors = newAcc }
    where
        appendAndSort newPref = reverse . sort . (newPref ++) 
