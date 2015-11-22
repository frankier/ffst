{-# OPTIONS -Wall #-}
{-# LANGUAGE DoAndIfThenElse #-}

module Main where

import System.Environment (getArgs)
import System.Exit (exitFailure)
import Parser (getFST)
import Data.Binary.Get (runGet)
import Control.Monad.Loops (unfoldrM)

import qualified Data.ByteString as BSS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.UTF8 as U8S
import qualified Data.Vector as V

import qualified Data
import qualified Simulator

usage :: IO ()
usage = putStrLn "Usage: ffst <HFST OL file>"

parseArgs :: [String] -> IO String
parseArgs [fn] = return fn
parseArgs _ = usage >> exitFailure

takeAlphabetIndex :: Data.FST -> BSS.ByteString -> Maybe (Maybe (Data.AlphabetIndex, BSS.ByteString))
takeAlphabetIndex transducer input
    | BSS.null input = Just Nothing
    | otherwise = Just <$> ((,) <$> alphabetIndex <*> rest)
  where
    alphabet = Data.inputAlphabet transducer
    alphabetIndex = V.findIndex (`BSS.isPrefixOf` input) alphabet
    symbol = (alphabet V.!) <$> alphabetIndex
    rest = flip BSS.drop input . BSS.length <$> symbol

bsToAlphabetString :: Data.FST -> BSS.ByteString -> Maybe [Data.AlphabetIndex]
bsToAlphabetString transducer = unfoldrM $ takeAlphabetIndex transducer

alphabetStringToBs :: Data.FST -> [Data.AlphabetIndex] -> BSS.ByteString
alphabetStringToBs transducer = BSS.concat . fmap (Data.alphabet transducer V.!)

repl :: Data.FST -> IO ()
repl transducer = repl'
  where
    repl' = do
      input <- BSS.getLine
      BSS.putStr $ humanRun input
      putStrLn ""
      repl'
    humanRun :: BSS.ByteString -> BSS.ByteString
    humanRun input =
      maybe
        (U8S.fromString "Input contains symbols not in transducer input alphabet")
        ((alphabetStringToBs transducer) . (Simulator.runFST transducer))
        (bsToAlphabetString transducer input)

main :: IO ()
main = do
    args <- getArgs
    fn <- parseArgs args
    input <- BSL.readFile fn
    let transducer = runGet getFST input
    --putStrLn . Data.fstToString $ transducer
    repl transducer
