{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}

module Lean where

import Control.DeepSeq
import Control.Monad
import Data.ByteString.Short (ShortByteString)
import Data.IORef
import GHC.Generics (Generic)
import System.IO

import qualified Data.Map.Strict as M

import Parser


data LeanLoc = LL {
    llFunName :: !Int,
    llFilePath :: !Int,
    llLn :: !Int,
    llCol :: !Int
} deriving (Eq, Generic, Ord, NFData, Show)

type LeanStack = [LeanLoc]

data LeanCodeEvent = LCE EventType !LeanLoc !LeanStack
   deriving (Eq, Generic, Ord, NFData, Show)

type LeanCallTrace = [LeanCodeEvent]

type StringsMap = (M.Map Int ShortByteString, M.Map ShortByteString Int)


emptyStringsMap :: StringsMap
emptyStringsMap = (M.empty, M.empty)

getSize :: IORef StringsMap -> IO Int
getSize mRef = do
    (_, m) <- readIORef mRef
    return $ M.size m

getId :: IORef StringsMap -> ShortByteString -> IO Int
getId mRef val = do
    (idToSt, stToId) <- readIORef mRef
    if val `M.member` stToId
        then return $ stToId M.! val
        else do
            let id = M.size stToId
            let newStToId = M.insert val id stToId
            let newIdToSt = M.insert id val idToSt
            let _ = rnf newIdToSt
            let _ = rnf newStToId
            writeIORef mRef $ (newIdToSt, newStToId)
            return id

printAllStrings :: IORef StringsMap -> IO ()
printAllStrings mRef = do
    (idToSt, stToId) <- readIORef mRef
    forM_ (M.keys idToSt) $ \k -> do
        putStrLn $ show k ++ " " ++ show (idToSt M.! k)
    forM_ (M.keys stToId) $ \k -> do
        putStrLn $ show k ++ " " ++ show (stToId M.! k)


toLeanLoc :: IORef StringsMap -> Loc -> IO LeanLoc
toLeanLoc mRef (CL fn fp l c) = do
    fnId <- getId mRef fn
    fpId <- getId mRef fp
    return $! LL fnId fpId l c

toLeanStack :: IORef StringsMap -> Stack -> IO LeanStack
toLeanStack mRef = mapM (toLeanLoc mRef)

toLeanCodeEvent :: IORef StringsMap -> CodeEvent -> IO LeanCodeEvent
toLeanCodeEvent mRef (CE et loc st) = do
    lLoc <- toLeanLoc mRef loc
    lSt <- toLeanStack mRef st
    let _ = rnf lLoc
    let _ = rnf lSt
    return $! LCE et lLoc lSt

toLeanCallTrace :: IORef StringsMap -> CallTrace -> IO LeanCallTrace
toLeanCallTrace mRef = mapM (toLeanCodeEvent mRef)
