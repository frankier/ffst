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
      BSS.putStr $ humanRun transducer input
      putStrLn ""
      repl'

humanRun :: Data.FST -> BSS.ByteString -> BSS.ByteString
humanRun transducer input =
    maybe
      (U8S.fromString "Input contains symbols not in transducer input alphabet")
      (\asInput -> mconcat $ asInput >>= flip (:) ["\n"])
      (runFstBsToBs input)
  where
    runFstBsToBs :: BSS.ByteString -> Maybe [BSS.ByteString]
    runFstBsToBs bsInput =
      runFstBs <$> Data.bsToAlphabetString transducer bsInput
    runFstBs :: [Data.AlphabetIndex] -> [BSS.ByteString]
    runFstBs alphaStr =
      Data.alphabetStringToBs transducer <$> Simulator.runFST transducer alphaStr

readFST :: String -> IO Data.FST
readFST fn =
    runGet getFST <$> BSL.readFile fn

main :: IO ()
main =
    getArgs >>=
    parseArgs >>=
    readFST >>=
    repl
