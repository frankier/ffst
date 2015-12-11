{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.HUnit
import qualified Data.ByteString as BSS
import qualified Ffst
import Debug.Trace

omorfiTest :: BSS.ByteString -> [BSS.ByteString] -> Test
omorfiTest word interpretations = TestCase $ do
  transducer <- Ffst.readFST "transducers/omorfi-omor.analyse.hfst"
  let output = traceShowId $ Ffst.runFstBsToBs transducer word
  Just interpretations @=? output

testPuhun :: Test
testPuhun = omorfiTest "puhun" [
  "[WORD_ID=puhua][UPOS=VERB][VOICE=ACT][MOOD=INDV][TENSE=PRESENT][PERS=SG1]"
  ]

testOnko :: Test
testOnko = omorfiTest "onko" [
  "[WORD_ID=olla][UPOS=AUX][VOICE=ACT][MOOD=INDV][TENSE=PRESENT][PERS=SG3][CLIT=KO]",
  "[WORD_ID=olla][UPOS=VERB][VOICE=ACT][MOOD=INDV][TENSE=PRESENT][PERS=SG3][CLIT=KO]"
  ]

testMitaan :: Test
testMitaan = omorfiTest "mitään" [
  "[WORD_ID=mikään][UPOS=PRON][SUBCAT=QUANTIFIER][CASE=PAR]"
  ]

testVittu :: Test
testVittu = omorfiTest "vittu" [
  "[WORD_ID=vittu][UPOS=INTJ]",
  "[WORD_ID=vittu][UPOS=NOUN][NUM=SG][CASE=NOM]"
  ]

testPitaisinkohan :: Test
testPitaisinkohan = omorfiTest "pitäisiköhän" [
  "[WORD_ID=pitää][UPOS=AUX][VOICE=ACT][MOOD=COND][PERS=SG3][CLIT=KO][CLIT=HAN]",
  "[WORD_ID=pitää][UPOS=VERB][VOICE=ACT][MOOD=COND][PERS=SG3][CLIT=KO][CLIT=HAN]"
  ]

main :: IO Counts
main = runTestTT $ TestList [testPuhun, testOnko, testMitaan, testVittu, testPitaisinkohan]