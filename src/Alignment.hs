{-# LANGUAGE Strict #-}

module Alignment where

import Control.Monad.State
import qualified Data.Map as M ( Map, empty, insertWith, insert, (!),
        size, keys, elemAt, null, delete, assocs, member )
import Data.List ( nub, (\\), sort, partition )
import Debug.Trace

import Lean
import Parser


data EventDiff = EDLeft LeanCodeEvent
               | EDCommon LeanCodeEvent
               | EDRight LeanCodeEvent
    deriving (Eq, Ord)

type TraceDiff = [EventDiff]
type Score = Integer

instance Show EventDiff where
    show (EDLeft (LCE eventType loc _)) =
        unlines ["     LEFT: ", "Event: " ++ show eventType, "Loc: " ++ show loc]
    show (EDCommon (LCE eventType loc _)) =
        unlines ["                    COMMON: ", "Event: " ++ show eventType, "Loc: " ++ show loc]
    show (EDRight (LCE eventType loc _)) =
        unlines ["                                   RIGHT: ", "Event: " ++ show eventType, "Loc: " ++ show loc]

alignTraces :: LeanCallTrace -> LeanCallTrace -> (Score, TraceDiff)
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
        align (leftEv@(LCE _ _ leftSt):leftTr) (rightEv@(LCE _ _ rightSt):rightTr) (score, acc) =
            if leftEv == rightEv
                then align leftTr rightTr (score + 1, ((EDCommon leftEv):acc))
            else if (length leftSt) <= (length rightSt)
                then align (leftEv:leftTr) rightTr (score, ((EDRight rightEv):acc))
            else align leftTr (rightEv:rightTr) (score, ((EDLeft leftEv):acc))


flipDiff :: TraceDiff -> TraceDiff
flipDiff = map flipEvent
  where
    flipEvent (EDLeft ev) = (EDRight ev)
    flipEvent (EDRight ev) = (EDLeft ev)
    flipEvent (EDCommon ev) = (EDCommon ev)


removeDuplicateTraces :: [LeanCallTrace] -> [LeanCallTrace] -> ([LeanCallTrace], [LeanCallTrace])
removeDuplicateTraces leftTraces rightTraces = do
    let leftUnique = leftTraces
    let rightUnique = rightTraces
    (leftUnique \\ rightUnique, rightUnique \\ leftUnique)


data SMPState = SMP {
    proposers :: M.Map Integer LeanCallTrace,
    acceptors :: M.Map Integer LeanCallTrace,
    preferences :: M.Map (Integer, Integer) (Score, TraceDiff),
    freeProposers :: M.Map Integer [(Score, Integer)],
    engagedProposers :: M.Map Integer [(Score, Integer)],
    engagedAcceptors :: M.Map Integer (Score, Integer),
    unmatchedProposers :: [Integer]
} deriving (Show)

type SMPMonad = State SMPState

initialSMPState :: SMPState
initialSMPState = SMP M.empty M.empty M.empty M.empty M.empty M.empty []

addProposer :: LeanCallTrace -> SMPMonad ()
addProposer tr = do
    st <- get
    let prop = proposers st
    let id = fromIntegral $ M.size prop
    put $ st { proposers = M.insert id tr prop }

addAcceptor :: LeanCallTrace -> SMPMonad ()
addAcceptor tr = do
    st <- get
    let acc = acceptors st
    let id = fromIntegral $ M.size acc
    put $ st { acceptors = M.insert id tr acc }

fillAllPreferences :: SMPMonad ()
fillAllPreferences = do
    st <- get
    let pairs = [(prop, acc) | prop <- M.keys (proposers st), acc <- M.keys (acceptors st)]
    mapM_ (uncurry fillPreference) pairs

fillPreference :: Integer -> Integer -> SMPMonad ()
fillPreference propId accId = do
    st <- get
    let prop = (proposers st) M.! propId
    let acc = (acceptors st) M.! accId
    let newPref@(score, _) = alignTraces prop acc
    put $ st { preferences = M.insert (propId, accId) newPref (preferences st)}

initFreeProposers :: SMPMonad ()
initFreeProposers = do
    st <- get
    mapM_ initFreeProposer $ M.keys (proposers st)

initFreeProposer :: Integer -> SMPMonad ()
initFreeProposer prop = do
    st <- get
    let prefMap = preferences st
    let prefs = [(fst (prefMap M.! (prop, acc)), acc) | acc <- M.keys (acceptors st)]
    let newFp = M.insert prop ((reverse . sort) prefs) (freeProposers st)
    put $ st { freeProposers = newFp }

initEngagedAcceptors :: SMPMonad ()
initEngagedAcceptors = do
    st <- get
    mapM_ initEngagedAcceptor $ M.keys (acceptors st)

initEngagedAcceptor :: Integer -> SMPMonad ()
initEngagedAcceptor acc = do
    st <- get
    put $ st { engagedAcceptors = M.insert acc (0, -1) (engagedAcceptors st)}

findMatches :: SMPMonad ([TraceDiff], [LeanCallTrace], [LeanCallTrace])
findMatches = do
    st <- get
    let fp = freeProposers st
    if M.null fp
        then retrieveMatches
        else do
            let (prop, pref) = M.elemAt 0 fp
            if null pref || (fst (head pref) == -1)
                then do
                    let newFp = M.delete prop fp
                    let up = unmatchedProposers st
                    put $ st { freeProposers = newFp, unmatchedProposers = (prop:up) }
                    findMatches
                else do
                    let (score, acc) = head pref
                    let ea = engagedAcceptors st
                    -- unless (M.member acc ea) $ error "error"
                    let (currentScore, currentProposer) = ea M.! acc
                    if currentScore >= score
                        then do
                            let newFp = M.insert prop (tail pref) fp
                            put $ st { freeProposers = newFp }
                            findMatches
                        else do
                            let ep = engagedProposers st
                            if currentProposer /= -1
                                then do
                                    let currentPref = ep M.! currentProposer
                                    let newFp = M.insert currentProposer (tail currentPref) $ M.delete prop fp
                                    let newEp = M.insert prop pref $ M.delete currentProposer ep
                                    let newEa = M.insert acc (score, prop) ea
                                    put $ st { freeProposers = newFp, engagedProposers = newEp, engagedAcceptors = newEa }
                                else do
                                    let newFp = M.delete prop fp
                                    let newEp = M.insert prop pref ep
                                    let newEa = M.insert acc (score, prop) ea
                                    put $ st { freeProposers = newFp, engagedProposers = newEp, engagedAcceptors = newEa }
                            findMatches

retrieveMatches :: SMPMonad ([TraceDiff], [LeanCallTrace], [LeanCallTrace])
retrieveMatches = do
    st <- get
    let ea = engagedAcceptors st
    let up = unmatchedProposers st
    let (matchedAcceptors, freeAcceptors) = partition filterByScore $ M.assocs ea
    let unmatchedPropTraces = map ((proposers st) M.!) up
    let unmatchedAccTraces = map (\(acc, _) -> (acceptors st) M.! acc) freeAcceptors
    let matchedTraces = map (\(acc, (_, prop)) -> snd $ (preferences st) M.! (prop, acc)) matchedAcceptors
    return (matchedTraces, unmatchedPropTraces, unmatchedAccTraces)
    where
        filterByScore (acc, (score, prop)) = score /= 0

findDiffs :: [LeanCallTrace] -> [LeanCallTrace] -> SMPMonad ([TraceDiff], [LeanCallTrace], [LeanCallTrace])
findDiffs leftTraces rightTraces = do
    let (leftTr, rightTr) = removeDuplicateTraces leftTraces rightTraces
    mapM_ addProposer leftTr
    mapM_ addAcceptor rightTr
    fillAllPreferences
    initFreeProposers
    initEngagedAcceptors
    findMatches
    retrieveMatches

filterNodiffs :: [TraceDiff] -> [TraceDiff]
filterNodiffs = filter isNotAllCommon
  where
    isNotAllCommon = any isNotCommon
    isNotCommon ed = case ed of
        EDCommon _ -> False
        _ -> True

analyzeTraces :: [LeanCallTrace] -> [LeanCallTrace] -> ([TraceDiff], [LeanCallTrace], [LeanCallTrace])
analyzeTraces leftTraces rightTraces = evalState (findDiffs leftTraces rightTraces) initialSMPState

