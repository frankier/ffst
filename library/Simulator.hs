module Simulator where

import Control.Arrow ((&&&), first)
import Data.Foldable (foldl')
import Text.Show.Pretty (ppShow)

import qualified Data.ByteString as BSS
import qualified Data.Set.Monad as Set
import qualified Data.Vector as V

import qualified Data
import OptimizedLookup

type FSTState = ([Data.AlphabetIndex], FSTCursor)
type FSTStates = Set.Set FSTState

startState :: FSTCursor
startState = TIACursor 0

isFinalState :: Data.FST -> FSTCursor -> Bool
isFinalState transducer (TIACursor tiaIdx) =
    tiaIn tiaRecord == Data.epsilonIdx && isFinalState transducer cursorNext
  where
    cursorNext = tiaNext tiaRecord
    tiaOffset = getTIAByteOffset (tiaIdx + Data.epsilonIdx)
    tiaRecord = getTiaRecord transducer tiaOffset

isFinalState _ (TTCursor 0) = False
isFinalState transducer (TTCursor ttIdx) =
    ttRecordIsFinal $ getTtRecord transducer ttOffset
  where
    ttOffset = getTtByteOffset transducer ttIdx
    ttRecordIsFinal (TTIsFinal isFinal) = isFinal
    ttRecordIsFinal _ =
      isFinalState transducer $ TTCursor $ ttIdx - 1

showStateEvo :: Data.FST -> [FSTStates] -> String
showStateEvo t stateEvo = unlines $ showStates t <$> stateEvo

showStates :: Data.FST -> FSTStates -> String
showStates t states = ppShow $ fmap (first $ Data.alphabetStringToBs t) states

runFST :: Data.FST -> [Data.AlphabetIndex] -> [[Data.AlphabetIndex]]
runFST transducer input =
    let
      expandedStartStates = expandEpsilon transducer (Set.singleton ([], startState))
      finalStates = runFST' expandedStartStates input
      acceptingFinalStates = Set.map fst $
        Set.filter (isFinalState transducer . snd) finalStates
    in
      Set.toList acceptingFinalStates
  where
    runFST' = foldl' (getNextStatesEpsilon transducer)

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
    movedStates = concatMapStatesApp ((flip $ getNextStates transducer) inSym) startStates

expandEpsilon :: Data.FST -> FSTStates -> FSTStates
expandEpsilon transducer =
    expandEpsilon' Set.empty
  where
    expandEpsilon' current new
      | new == Set.empty = current
      | otherwise =
          let next = concatMapStatesApp ((flip $ getNextStates transducer) Data.epsilonIdx) new
          in expandEpsilon' (Set.union current new) next

tc :: Data.FST -> Data.AlphabetIndex -> BSS.ByteString
tc transducer idx = Data.alphabet transducer V.! idx

getNextStates :: Data.FST -> FSTCursor -> Data.AlphabetIndex -> Set.Set (Data.AlphabetIndex, FSTCursor)
getNextStates transducer (TIACursor tiaIdx) inSym
    | matches = getNextStates transducer cursorNext inSym
    | otherwise = Set.empty
  where
    tiaOffset = getTIAByteOffset (tiaIdx + inSym)
    tiaRecord = getTiaRecord transducer tiaOffset
    recIn = tiaIn tiaRecord
    matches = recIn == inSym
    cursorNext = tiaNext tiaRecord

getNextStates transducer (TTCursor ttIdx) inSym =
    Set.fromList $ (ttOut &&& ttNext) <$> edgeRecords
  where
    ttOffset = getTtByteOffset transducer ttIdx
    edgeRecords = getTtRecords transducer ttOffset inSym
