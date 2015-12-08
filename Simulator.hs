module Simulator where

import Debug.Trace
--import Hexdump

import Control.Arrow ((&&&), first)
import Text.Show.Pretty (ppShow)

--import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BSS
import qualified Data.Set.Monad as Set
import qualified Data.Vector as V

import qualified Data
import OptimizedLookup

data FSTCursor = TIACursor Int | TTCursor Int deriving (Eq, Show, Ord)
type FSTState = ([Data.AlphabetIndex], FSTCursor)
type FSTStates = Set.Set FSTState

startState :: FSTCursor
startState = TTCursor 0

isFinalState :: Data.FST -> FSTCursor -> Bool
isFinalState _ (TIACursor _) = False
isFinalState _ (TTCursor 0) = False
isFinalState transducer (TTCursor ttIdx) =
    ttRecordIsFinal $ getTtRecord transducer ttOffset
  where
    ttOffset = getTtByteOffset transducer ttIdx
    ttRecordIsFinal (TTIsFinal isFinal) = traceShow ("isfinal", ttIdx, isFinal) isFinal
    ttRecordIsFinal ttr =
      traceShow ("notisfinal", ttIdx, ttr)
        isFinalState transducer $ TTCursor $ ttIdx - 1

--
--trace (simpleHex . BSL.toStrict . BSL.take 1024 $ Data.tt transducer) input

showStateEvo :: Data.FST -> [FSTStates] -> String
showStateEvo t stateEvo = ppShow $ fmap (fmap (first $ Data.alphabetStringToBs t)) stateEvo

runFST :: Data.FST -> [Data.AlphabetIndex] -> [[Data.AlphabetIndex]]
runFST transducer input =
    let
      statesEvolution = runFST' (Set.singleton ([], startState)) input
      finalStates = last statesEvolution
      acceptingFinalStates = traceShowId $ Set.map fst $
        Set.filter (isFinalState transducer . snd) finalStates
    in
      -- trace (prettyHex . BSL.toStrict . BSL.take 1024 $ Data.tia transducer)
      -- $
      traceShow (finalStates, acceptingFinalStates, showStateEvo transducer statesEvolution)
                (Set.toList acceptingFinalStates)
  where
    runFST' states (h:rest) =
      let nextStates = getNextStatesEpsilon transducer states h
      in states:runFST' nextStates rest
    runFST' states [] = [states]

concatMapStates :: ([Data.AlphabetIndex] -> a -> [Data.AlphabetIndex]) -> (FSTCursor -> Set.Set (a, FSTCursor)) -> FSTStates -> FSTStates
concatMapStates combineStrs f states =
    expandStates =<< states
  where
    expandStates (oldStr, oldCur) =
      let mapStates (newBit, newCur) =
            (combineStrs oldStr newBit, newCur)
      in mapStates <$> f oldCur

concatMapStatesApp :: (FSTCursor -> Set.Set (Data.AlphabetIndex, FSTCursor)) -> FSTStates -> FSTStates
concatMapStatesApp = concatMapStates (\x y -> x ++ [y])

concatMapStatesJoin :: (FSTCursor -> Set.Set ([Data.AlphabetIndex], FSTCursor)) -> FSTStates -> FSTStates
concatMapStatesJoin = concatMapStates (++)

getNextStatesEpsilon :: Data.FST -> FSTStates -> Data.AlphabetIndex -> FSTStates
getNextStatesEpsilon transducer startStates inSym =
    expandEpsilon transducer movedStates
  where
    expandedStartStates = expandEpsilon transducer startStates
    movedStates = concatMapStatesApp ((flip $ getNextStates transducer) inSym) expandedStartStates

expandEpsilon :: Data.FST -> FSTStates -> FSTStates
expandEpsilon transducer states =
    expandEpsilon' states Set.empty
  where
    expandEpsilon' current checked
      | current == checked = current
      | otherwise =
          let next = concatMapStatesApp ((flip $ getNextStates transducer) Data.epsilonIdx) current
          in expandEpsilon' (Set.union current next) current

tc :: Data.FST -> Data.AlphabetIndex -> BSS.ByteString
tc transducer idx = Data.alphabet transducer V.! idx

getNextStates :: Data.FST -> FSTCursor -> Data.AlphabetIndex -> Set.Set (Data.AlphabetIndex, FSTCursor)
getNextStates transducer (TIACursor tiaIdx) inSym
    | matches = getNextStates transducer cursorNext inSym
    | otherwise = Set.empty
  where
    tiaOffset = getTIAByteOffset (traceShow ("TIA IDX", tiaIdx, tiaIdx + inSym) (tiaIdx + inSym))
    tiaRecord = trace "tiaRecord" $ traceShowId $ getTiaRecord transducer tiaOffset
    recIn = tiaIn tiaRecord
    matches = traceShow ("TIA array lookup", recIn, inSym, tc transducer inSym) (recIn == inSym)
    idxNext = tiaIdxNext tiaRecord
    cursorNext
      | tiaIsTt tiaRecord = TTCursor idxNext
      | otherwise = TIACursor idxNext

getNextStates transducer (TTCursor ttIdx) inSym =
    Set.fromList $ (ttOut &&& nextCursor) <$> edgeRecords
  where
    ttOffset = getTtByteOffset transducer (traceShow ("TT IDX", ttIdx) ttIdx)
    --isFinal = ttrsIsFinal ttRecords
    edgeRecords = ttrsEdgeRecords ttRecords
    ttRecords = traceShowId $ getTtRecords transducer ttOffset inSym
    nextCursor tt
      | ttIsTt tt = TTCursor $ ttIdxNext tt
      | otherwise = TIACursor $ ttIdxNext tt
