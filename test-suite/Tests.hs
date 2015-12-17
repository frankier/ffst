module Main where

import Test.HUnit
import qualified Data.ByteString as BSS
import qualified Data.ByteString.UTF8 as U8S
import qualified Ffst

omorfiTest :: BSS.ByteString -> [BSS.ByteString] -> Test
omorfiTest word interpretations = TestCase $ do
  transducer <- Ffst.readFST "transducers/omorfi-omor.analyse.hfst"
  let output = Ffst.runFstBsToBs transducer word
  Just interpretations @=? output

testPuhun :: Test
testPuhun = omorfiTest (U8S.fromString "puhun") [
  U8S.fromString "[WORD_ID=puhua][UPOS=VERB][VOICE=ACT][MOOD=INDV][TENSE=PRESENT][PERS=SG1]"
  ]

testOnko :: Test
testOnko = omorfiTest (U8S.fromString "onko") [
  U8S.fromString "[WORD_ID=olla][UPOS=AUX][VOICE=ACT][MOOD=INDV][TENSE=PRESENT][PERS=SG3][CLIT=KO]",
  U8S.fromString "[WORD_ID=olla][UPOS=VERB][VOICE=ACT][MOOD=INDV][TENSE=PRESENT][PERS=SG3][CLIT=KO]"
  ]

testMitaan :: Test
testMitaan = omorfiTest  (U8S.fromString "mitään") [
  U8S.fromString "[WORD_ID=mikään][UPOS=PRON][SUBCAT=QUANTIFIER][CASE=PAR]"
  ]

testVittu :: Test
testVittu = omorfiTest (U8S.fromString "vittu") [
  U8S.fromString "[WORD_ID=vittu][UPOS=INTJ]",
  U8S.fromString "[WORD_ID=vittu][UPOS=NOUN][NUM=SG][CASE=NOM]"
  ]

testPitaisinkohan :: Test
testPitaisinkohan = omorfiTest (U8S.fromString "pitäisiköhän") [
  U8S.fromString "[WORD_ID=pitää][UPOS=AUX][VOICE=ACT][MOOD=COND][PERS=SG3][CLIT=KO][CLIT=HAN]",
  U8S.fromString "[WORD_ID=pitää][UPOS=VERB][VOICE=ACT][MOOD=COND][PERS=SG3][CLIT=KO][CLIT=HAN]"
  ]


main :: IO Counts
main = runTestTT $ TestList [testPuhun, testOnko, testMitaan, testVittu, testPitaisinkohan]
