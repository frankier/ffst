{-# OPTIONS -Wall #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment (getArgs)
import System.Exit (exitFailure)
import Parser (getFST)
import Data.Binary.Get (runGet)

import qualified Data.ByteString as BSS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.UTF8 as U8S
--import qualified Data.Set.Monad as Set
--import qualified Data.Vector as V

import qualified Data
import qualified Simulator

usage :: IO ()
usage = putStrLn "Usage: ffst <HFST OL file>"

parseArgs :: [String] -> IO String
parseArgs [fn] = return fn
parseArgs _ = usage >> exitFailure

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
        (\asInput -> mconcat $ runFstBs asInput >>= flip (:) ["\n"])
        (Data.bsToAlphabetString transducer input)
    runFstBs :: [Data.AlphabetIndex] -> [BSS.ByteString]
    runFstBs input =
      Data.alphabetStringToBs transducer <$> Simulator.runFST transducer input

main :: IO ()
main = do
    args <- getArgs
    fn <- parseArgs args
    input <- BSL.readFile fn
    let transducer = runGet getFST input
    putStrLn . Data.fstToString $ transducer
    repl transducer
