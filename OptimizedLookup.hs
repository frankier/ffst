{-# LANGUAGE DoAndIfThenElse #-}

module OptimizedLookup where

import Data.Binary.Get
import Data.Binary.IEEE754
import Data.Bits
import Data.Maybe

import Control.Applicative
import Control.Monad.Loops (unfoldWhileM)

import qualified Data
import Utils

data FSTCursor = TIACursor Int | TTCursor Int deriving (Eq, Show, Ord)

data TIARecord = TIARecord {
  tiaIn :: Data.AlphabetIndex,
  tiaNext :: FSTCursor
} deriving (Show, Eq)

data TTEdgeRecord = TTEdgeRecord {
  ttIn :: Data.AlphabetIndex,
  ttOut :: Data.AlphabetIndex,
  ttNext :: FSTCursor,
  ttWeight :: Float
} deriving (Show, Eq)

data TTRecord = TTER TTEdgeRecord | TTIsFinal Bool deriving (Show, Eq)

getTIAByteOffset :: Int -> Int
getTIAByteOffset offset = (offset + 1) * 6

getTtByteOffset :: Data.FST -> Int -> Int
getTtByteOffset transducer offset
  | Data.isWeighted $ Data.olHeader transducer = offset * 4 * 3
  | otherwise = offset * 4 * 2

getTiaRecord :: Data.FST -> Int -> TIARecord
getTiaRecord transducer offset =
    runGet (skip offset *> getTiaRecordGet) $ Data.tia transducer

isTtBit :: Int
isTtBit = 1 `shift` 31

getFstCursor :: Get FSTCursor
getFstCursor =
    nextToFstCursor <$> getWord32leAsNum
  where
    nextToFstCursor next
      | next .&. isTtBit > 0 = TTCursor (next `xor` isTtBit)
      | otherwise = TIACursor next

getTiaRecordGet :: Get TIARecord
getTiaRecordGet =
  TIARecord <$>
    getWord16leAsNum  <*>
    getFstCursor

getTtRecord :: Data.FST -> Int -> TTRecord
getTtRecord transducer offset =
    runGet (skip offset *> getTtRecordGet transducer) $ Data.tt transducer

getIsTtFinal :: Get Bool
getIsTtFinal = do
  isFinalityRecord <- getWord32leAsNum
  if (isFinalityRecord :: Int) == 0xffffffff
  then (== 1) <$> getWord32le <* getFloat32le
  else fail ""

getTtRecordGet :: Data.FST -> Get TTRecord
getTtRecordGet _ =
  (TTIsFinal <$> getIsTtFinal) <|>
  TTER <$>
    (TTEdgeRecord <$>
      getWord16leAsNum  <*>
      getWord16leAsNum  <*>
      getFstCursor <*>
      getFloat32le)

getTtRecords :: Data.FST -> Int -> Data.AlphabetIndex -> [TTEdgeRecord]
getTtRecords transducer offset inSym =
  runGet (skip offset *> getTtRecordListGet transducer inSym) (Data.tt transducer)

getTtRecordListGet :: Data.FST -> Data.AlphabetIndex -> Get [TTEdgeRecord]
getTtRecordListGet transducer inSym =
    mapMaybe getTter <$> filter matchesInput <$> ((:) <$> getTT <*> unfoldWhileM matchesInput getTT)
  where
    getTT = getTtRecordGet transducer
    matchesInput (TTER tter) = ttIn tter == inSym
    matchesInput _ = False
    getTter (TTER x) = Just x
    getTter _ = Nothing
