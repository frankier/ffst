{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.HUnit
import qualified Data.ByteString as BSS
import qualified Ffst
import Data.String.UTF8

transducerTest :: String -> BSS.ByteString -> [BSS.ByteString] -> Test
transducerTest tranducerFn word interpretations = TestCase $ do
  transducer <- Ffst.readFST tranducerFn
  let output = Ffst.runFstBsToBs transducer word
  Just interpretations @=? output

omorfiTest = transducerTest "transducers/omorfi-omor.analyse.hfst"
englishTest = transducerTest "transducers/english-bnc.hfstol"
frenchTest = transducerTest "transducers/omorfi-omor.analyse.hfst"

testPuhun :: Test
testPuhun = omorfiTest (toRep "puhun") [
  toRep "[WORD_ID=puhua][UPOS=VERB][VOICE=ACT][MOOD=INDV][TENSE=PRESENT][PERS=SG1]"
  ]

testOnko :: Test
testOnko = omorfiTest (toRep "onko") [
  toRep "[WORD_ID=olla][UPOS=AUX][VOICE=ACT][MOOD=INDV][TENSE=PRESENT][PERS=SG3][CLIT=KO]",
  toRep "[WORD_ID=olla][UPOS=VERB][VOICE=ACT][MOOD=INDV][TENSE=PRESENT][PERS=SG3][CLIT=KO]"
  ]

testMitaan :: Test
testMitaan = omorfiTest  (toRep "mitään") [
  toRep "[WORD_ID=mikään][UPOS=PRON][SUBCAT=QUANTIFIER][CASE=PAR]"
  ]

testVittu :: Test
testVittu = omorfiTest (toRep "vittu") [
  toRep "[WORD_ID=vittu][UPOS=INTJ]",
  toRep "[WORD_ID=vittu][UPOS=NOUN][NUM=SG][CASE=NOM]"
  ]

testPitaisinkohan :: Test
testPitaisinkohan = omorfiTest (toRep "pitäisiköhän") [
  toRep "[WORD_ID=pitää][UPOS=AUX][VOICE=ACT][MOOD=COND][PERS=SG3][CLIT=KO][CLIT=HAN]",
  toRep "[WORD_ID=pitää][UPOS=VERB][VOICE=ACT][MOOD=COND][PERS=SG3][CLIT=KO][CLIT=HAN]"
  ]

testJyvaskylassa :: Test
testJyvaskylassa = omorfiTest (toRep "Jyväskylässä") [
  toRep "[WORD_ID=Jyväskylä][UPOS=PROPN][PROPER=PROPER][NUM=SG][CASE=INE]"
  ]

testPuhelimet :: Test
testPuhelimet = omorfiTest (toRep "puhelimet") [
  toRep "[WORD_ID=puhe][UPOS=NOUN][NUM=SG][CASE=NOM][BOUNDARY=COMPOUND][WORD_ID=lime][UPOS=NOUN][NUM=PL][CASE=NOM]",
  toRep "[WORD_ID=puhelin][UPOS=NOUN][NUM=PL][CASE=NOM]"
  ]

testDoors :: Test
testDoors = englishTest (toRep "doors") [
  toRep "door<NN2>"
  ]

testFoxes :: Test
testFoxes = englishTest (toRep "foxes") [
  toRep "fox<NN2-VVZ>",
  toRep "fox<NN2>",
  toRep "fox<VVZ-NN2>",
  toRep "fox<VVZ>"
  ]

testHung :: Test
testHung = englishTest (toRep "hung") [
  toRep "hang<VVD-AJ0>",
  toRep "hang<VVD-VVN>",
  toRep "hang<VVD>",
  toRep "hang<VVN-AJ0>",
  toRep "hang<VVN-VVD>",
  toRep "hang<VVN>",
  toRep "hung<AJ0-VVD>",
  toRep "hung<AJ0-VVN>",
  toRep "hung<AJ0>"
  ]

testHanged :: Test
testHanged = englishTest (toRep "hanged") [
  toRep "hang<VVD-VVN>",
  toRep "hang<VVD>",
  toRep "hang<VVN-AJ0>",
  toRep "hang<VVN>",
  toRep "hanged<AJ0-VVD>",
  toRep "hanged<AJ0-VVN>",
  toRep "hanged<AJ0>"
  ]

testAntidisestablishmentarianism :: Test
testAntidisestablishmentarianism = englishTest (toRep "antidisestablishmentarianism") [
  toRep "antidisestablishmentarianism<NN1>"
  ]

main :: IO Counts
main = runTestTT $ TestList [testPuhun, testOnko, testMitaan, testVittu, testPitaisinkohan, testJyvaskylassa, testPuhelimet]
