module Simulator where

import qualified Data

startState :: Data.FSTCursor
startState = Data.TTCursor 0

isFinalState :: Data.FSTCursor -> Bool
isFinalState _ = False

runFST :: Data.FST -> [Data.AlphabetIndex] -> [Data.AlphabetIndex]
runFST transducer input = input

getNextStates :: Data.FST -> Data.FSTCursor -> Data.AlphabetIndex -> [(Data.AlphabetIndex, Data.FSTCursor)]
getNextStates transducer (Data.TIACursor tiaIdx) inSym =
    [(inSym, (Data.TIACursor tiaIdx))]
    --tiaIdx + inSym

getNextStates transducer (Data.TTCursor ttIdx) inSym =
    [(inSym, (Data.TTCursor ttIdx))]
