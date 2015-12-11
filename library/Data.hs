{-# OPTIONS -Wall #-} 
{-# LANGUAGE OverloadedStrings #-}

module Data where

import Text.Show.Pretty (ppShow)
import Control.Monad.Loops (unfoldrM)

import qualified Data.ByteString.Lazy.UTF8 as U8L
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BSS
import qualified Data.Map.Strict as SM
import qualified Data.Vector as V

type BSSMap = SM.Map String String
type Alphabet = V.Vector BSS.ByteString
type AlphabetIndex = Int

data OLHeader = OLHeader {
  inputSymbols :: !Int,
  totalSymbols :: !Int,
  tiaEntries :: !Int,
  ttEntries :: !Int,
  states :: !Int,

  isWeighted :: !Bool,
  isDeterministic :: !Bool,
  isInputDeterministic  :: !Bool,
  isMinimized :: !Bool,
  isCyclic :: !Bool,
  hasEpsilonEpsilonTransitions :: !Bool,
  hasInputEpsilonTransitions :: !Bool,
  hasInputEpsilonCycles :: !Bool,
  hasUnweightedInputEpsilonCycles :: !Bool
} deriving (Show, Eq)

data FST = FST {
  hfstHeader :: !BSSMap,
  olHeader :: !OLHeader,
  hasFlagDiacritics :: !Bool,
  alphabet :: !Alphabet,
  tia :: BSL.ByteString,
  tt :: BSL.ByteString
} deriving (Eq, Show)

fstToString :: FST -> String
fstToString transducer =
    ppShow $ transducer {tia = ellipses, tt = ellipses}
  where
    ellipses = U8L.fromString "..."

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

epsilonIdx :: Int
epsilonIdx = 0

takeAlphabetIndex :: FST -> BSS.ByteString -> Maybe (Maybe (AlphabetIndex, BSS.ByteString))
takeAlphabetIndex transducer input
    | BSS.null input = Just Nothing
    | otherwise = Just <$> ((,) <$> alphabetIndex <*> rest)
  where
    abc = inputAlphabet transducer
    alphabetIndex = V.findIndex (`BSS.isPrefixOf` input) abc
    symbol = (abc V.!) <$> alphabetIndex
    rest = flip BSS.drop input . BSS.length <$> symbol

bsToAlphabetString :: FST -> BSS.ByteString -> Maybe [AlphabetIndex]
bsToAlphabetString transducer = unfoldrM $ takeAlphabetIndex transducer

alphabetStringToBs :: FST -> [AlphabetIndex] -> BSS.ByteString
alphabetStringToBs transducer =
    BSS.concat . fmap indexToBs
  where
    indexToBs 0 = "" -- should be epsilonIdx, cannot match already bound names
    indexToBs i = Data.alphabet transducer V.! i
