{-# LANGUAGE DoAndIfThenElse #-}

module OptimizedLookup where

import Data.Binary.Get
import Data.Binary.IEEE754
import Data.Maybe
import Debug.Trace
import Hexdump

import Control.Applicative
import Control.Monad.Loops (unfoldWhileM)

import qualified Data.ByteString.Lazy as BSL

import qualified Data
import Utils

data TIARecord = TIARecord {
  tiaIn :: Int,
  tiaIdxNext :: Int,
  tiaIsTt :: Bool
} deriving (Show, Eq)

data TTEdgeRecord = TTEdgeRecord {
  ttIn :: Int,
  ttOut :: Int,
  ttIdxNext :: Int,
  ttIsTt :: Bool,
  ttWeight :: Float
} deriving (Show, Eq)

data TTRecord = TTER TTEdgeRecord | TTIsFinal Bool deriving (Show, Eq)

data TTRecords = TTRecords {
  ttrsIsFinal :: Bool,
  ttrsEdgeRecords :: [TTEdgeRecord]
} deriving (Show, Eq)

getTIAByteOffset :: Int -> Int
getTIAByteOffset offset = (offset + 1) * 6

getTtByteOffset :: Data.FST -> Int -> Int
getTtByteOffset transducer offset
  | Data.isWeighted $ Data.olHeader transducer = offset * 4 * 3
  | otherwise = offset * 4 * 2

getTiaRecord :: Data.FST -> Int -> TIARecord
getTiaRecord transducer offset =
    runGet (skip offset *> getTiaRecordGet) $ Data.tia transducer

getTiaRecordGet :: Get TIARecord
getTiaRecordGet =
  TIARecord <$>
    getWord16leAsNum  <*>
    getWord16leAsNum  <*>
    getBool16

getTtRecord :: Data.FST -> Int -> TTRecord
getTtRecord transducer offset =
    runGet (skip offset *> getTtRecordGet transducer) $ Data.tt transducer

getIsTtFinal :: Get Bool
getIsTtFinal = do
  isFinalityRecord <- getWord32leAsNum
  if (isFinalityRecord :: Int) == 0xffffffff
  then
    do
      isFinal <- getWord32le
      x <- getFloat32le
      return $ traceShow (isFinalityRecord, isFinal, x)  isFinal == 1
  else fail ""

getTtRecordGet :: Data.FST -> Get TTRecord
getTtRecordGet _ =
  (TTIsFinal <$> getIsTtFinal) <|>
  TTER <$>
    (TTEdgeRecord <$>
      getWord16leAsNum  <*>
      getWord16leAsNum  <*>
      getWord16leAsNum  <*>
      getBool16 <*>
      getFloat32le)

getTtRecords :: Data.FST -> Int -> Data.AlphabetIndex -> TTRecords
getTtRecords transducer offset inSym =
  trace (prettyHex . BSL.toStrict . BSL.take 128 . BSL.drop (fromIntegral offset) $ Data.tt transducer)
  runGet (skip offset *> getTtRecordsGet transducer inSym) (Data.tt transducer)

getTtRecordListGet :: Data.FST -> Get [TTRecord]
getTtRecordListGet transducer =
    (:) <$> getTT <*> unfoldWhileM isTter getTT
  where
    getTT = getTtRecordGet transducer
    isTter (TTER _) = True
    isTter _ = False

getTtRecordsGet :: Data.FST -> Data.AlphabetIndex -> Get TTRecords
getTtRecordsGet transducer inSym = do
    ttRecordList <- getTtRecordListGet transducer
    let isFinal = any isFinalTT ttRecordList
    let edgeRecords = mapMaybe getTter ttRecordList
    let relevantEdgeRecords  = filter (\x -> ttIn x == inSym) edgeRecords
    return (TTRecords isFinal relevantEdgeRecords)
  where
    isFinalTT (TTIsFinal x) = x
    isFinalTT _ = False
    getTter (TTER x) = Just x
    getTter _ = Nothing
