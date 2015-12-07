module Parser where

import Prelude hiding (any, elem)
import Data.Foldable (any, elem)

import Data.Binary.Get

import Control.Monad.Loops (untilM)
import Control.Error.Safe (tryAssert, tryJust)
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Control.Monad.Trans.Class (lift)

import qualified Data.ByteString as BSS
import qualified Data.Map.Strict as SM
import qualified Data.ByteString.UTF8 as U8S
import qualified Data.Vector as V

import qualified Data
import Utils

getMap :: Int -> Get Data.BSSMap
getMap mapBytes = isolate mapBytes getMap'
  where
    getMap' = SM.fromList <$> untilM getByteStringPair noBytesLeft
    getByteStringPair = azip getStringNul getStringNul
    noBytesLeft = (>= mapBytes) . fromIntegral <$> bytesRead

deserialiseHFSTHeader :: Get Data.BSSMap
deserialiseHFSTHeader =
    runExceptT deserialiseHFSTHeader' >>= either fail return
  where
    deserialiseHFSTHeader' :: ExceptT String Get Data.BSSMap
    deserialiseHFSTHeader' = do
      hfst <- lift getStringNul
      tryAssert "expected HFST" (hfst == "HFST")
      hlen <- lift getWord16le
      nul <- lift getWord8
      tryAssert "Expected zero padding byte after header length" (nul == 0)
      hfstMap <- lift . getMap $ fromIntegral hlen
      hfstType <- tryJust "No type field supplied in header" (SM.lookup "type" hfstMap)
      tryAssert "File type must be HFST_OLW" (hfstType == "HFST_OLW")
      hfstVersion <- tryJust "No version field supplied in header" (SM.lookup "version" hfstMap)
      tryAssert "Version must be 3.3" (hfstVersion == "3.3")
      return hfstMap

deserialiseOLHeader :: Get Data.OLHeader
deserialiseOLHeader =
  Data.OLHeader <$>
    getWord16leAsNum <*>
    getWord16leAsNum <*>
    getWord32leAsNum <*>
    getWord32leAsNum <*>
    getWord32leAsNum <*
    getBool32 <*> -- padding

    getBool32 <*>
    getBool32 <*>
    getBool32 <*>
    getBool32 <*>
    getBool32 <*>
    getBool32 <*>
    getBool32 <*>
    getBool32 <*>
    getBool32

getAlphabet :: Int -> Get Data.Alphabet
getAlphabet size = V.replicateM size getByteStringNul

isFlagDiacritic :: BSS.ByteString -> Bool
isFlagDiacritic symbol =
    length u8Symbol > 4 &&
    head u8Symbol == '@' &&
    u8Symbol !! 1 `elem` "PNRDCU" &&
    u8Symbol !! 2 == '.' &&
    last u8Symbol == '@'
  where
    u8Symbol = U8S.toString symbol

checkHasFlagDiacritics :: Data.Alphabet -> Bool
checkHasFlagDiacritics = any isFlagDiacritic

getFST :: Get Data.FST
getFST = do
  hfstHeader <- deserialiseHFSTHeader
  olHeader <- deserialiseOLHeader
  let alphabetSymbols = Data.totalSymbols olHeader
  alphabet <- getAlphabet alphabetSymbols 
  let hasFlagDiacritics = checkHasFlagDiacritics alphabet
  tia <- getLazyByteString . fromIntegral $ Data.tiaEntries olHeader * 6
  tt <- getRemainingLazyByteString
  return Data.FST {
    Data.hfstHeader = hfstHeader,
    Data.olHeader = olHeader,
    Data.hasFlagDiacritics = hasFlagDiacritics,
    Data.alphabet = alphabet,
    Data.tia = tia,
    Data.tt = tt
  }
