{-# OPTIONS -Wall #-} 

module Data where

import Text.Show.Pretty (ppShow)

import qualified Data.ByteString.UTF8 as U8S
import qualified Data.ByteString as BSS
import qualified Data.Map.Strict as SM
import qualified Data.Vector as V

type BSSMap = SM.Map String String
type Alphabet = V.Vector BSS.ByteString
type AlphabetIndex = Int

data OLHeader = OLHeader {
  inputSymbols :: Int,
  totalSymbols :: Int,
  tiaEntries :: Int,
  ttEntries :: Int,
  states :: Int,

  isWeighted :: Bool,
  isDeterministic :: Bool,
  isInputDeterministic  :: Bool,
  isMinimized :: Bool,
  isCyclic :: Bool,
  hasEpsilonEpsilonTransitions :: Bool,
  hasInputEpsilonTransitions :: Bool,
  hasInputEpsilonCycles :: Bool,
  hasUnweightedInputEpsilonCycles :: Bool
} deriving (Show, Eq)

data FST = FST {
  hfstHeader :: BSSMap,
  olHeader :: OLHeader,
  hasFlagDiacritics :: Bool,
  alphabet :: Alphabet,
  tia :: BSS.ByteString,
  tt :: BSS.ByteString
} deriving (Eq, Show)

data FSTCursor = TIACursor Int | TTCursor Int

data FSTState = FSTState {
  position :: FSTCursor,
  output :: String
}

fstToString :: FST -> String
fstToString transducer =
    ppShow $ transducer {tia = ellipses, tt = ellipses}
  where 
    ellipses = U8S.fromString "..."

inputAlphabet :: FST -> Alphabet
inputAlphabet transducer = V.slice 0 inputNum $ alphabet transducer
  where
    inputNum = inputSymbols $ olHeader transducer

outputAlphabet :: FST -> Alphabet
outputAlphabet transducer = V.slice inputNum outputNum $ alphabet transducer
  where
    header = olHeader transducer
    inputNum = inputSymbols header
    outputNum = totalSymbols header - inputNum
