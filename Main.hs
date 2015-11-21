{-# OPTIONS -Wall #-}
{-# LANGUAGE DoAndIfThenElse #-}

module Main where

import Control.Applicative
import Control.Monad
import Data.Binary.Get

import Prelude hiding (any, elem)
import Data.Foldable (any, elem)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import Debug.Trace (trace, traceShow)
import Control.Monad.Loops (untilM)
import Data.Functor.Compose (Compose, getCompose)
import Data.Either (either)
import Control.Error.Safe (tryAssert, tryJust)
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Control.Monad.Trans.Class (lift)

import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BSS
import qualified Data.Map.Strict as SM
--import qualified Data.ByteString.Lazy.UTF8 as U8L
import qualified Data.ByteString.UTF8 as U8S
import qualified Data.Vector as V

import qualified Data

bssMapToStringMap :: SM.Map BSS.ByteString BSS.ByteString -> SM.Map String String
bssMapToStringMap bssMap = SM.map U8S.toString $ SM.mapKeys U8S.toString bssMap

getByteStringNul :: Get (SM.Map BSS.ByteString BSS.ByteString)
getByteStringNul = BSL.toStrict <$> getLazyByteStringNul

getStringNul :: Get BSS.ByteString
getStringNul = bssMapToStringMap <$> getByteString

getBool32 :: Get Bool
getBool32 = fmap (/= 0) getWord32le

getWord16leAsNum :: (Num a) => Get a
getWord16leAsNum =
  fmap fromIntegral getWord16le

getWord32leAsNum :: (Num a) => Get a
getWord32leAsNum = fmap fromIntegral getWord32le

azip :: (Applicative f) => f a -> f b -> f (a, b)
azip fa fb = mkTuple2 <$> fa <*> fb
  where mkTuple2 a b = (a, b)

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
      tryAssert "Version must be 3.3" (hfstType == "3.3")
      return hfstMap

deserialiseOLHeader :: Get Data.OLHeader
deserialiseOLHeader =
  trace "deserialiseOLHeader" Data.OLHeader <$>
    getWord16leAsNum <*>
    getWord16leAsNum <*>
    getWord32leAsNum <*>
    getWord32leAsNum <*>
    getWord32leAsNum <*>

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
  tia <- getByteString . fromIntegral $ Data.tiaEntries olHeader * 6
  tt <- fmap BSL.toStrict getRemainingLazyByteString
  return Data.FST {
    Data.hfstHeader = hfstHeader,
    Data.olHeader = olHeader,
    Data.hasFlagDiacritics = hasFlagDiacritics,
    Data.alphabet = alphabet,
    Data.tia = tia,
    Data.tt = tt
  }

usage :: IO ()
usage = putStrLn "Usage: ffst <HFST OL file>"

parseArgs :: [String] -> IO String
parseArgs [fn] = return fn
parseArgs _ = usage >> exitFailure

main :: IO ()
main = do
  args <- getArgs
  fn <- parseArgs args
  input <- BSL.readFile fn
  let header = runGet getFST input
  putStrLn . Data.fstToString $ header
