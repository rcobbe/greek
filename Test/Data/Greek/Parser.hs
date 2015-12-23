module Test.Data.Greek.Parser(tests) where

import Prelude hiding (Word)
import qualified Control.Exceptional as CE
import Control.Exceptional.HUnit
import qualified Data.List as List
import qualified Data.Set as Set

import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as Text

import Test.HUnit
import qualified Test.QuickCheck as QC
import Test.Data.Greek.LetterGen
import Test.Utils

import Data.Greek.Word
import qualified Data.Greek.Parser as GP
import Data.Greek.Normalize
import Data.Greek.Output
import Data.Greek.UnicodeData

tests =
  "Greek.Parser" ~:
  [validLetterTests, randomWordTests, invalidLetterTests, errorTests]

-- | Tests parsing all supported letters, individually.
validLetterTests =
  "parsing individual letters" ~:
  map makeSuccessfulParseTest validCombinations
  where makeSuccessfulParseTest (macron, base, breathing, accent, iotaSub) =
          let letter = makeLetter base breathing accent iotaSub macron
              input = (letterToUnicode letter) :: String
          in (show letter ~: assertNoExn letter (GP.letter input))

invalidLetterTests =
  "invalid letter tests" ~:
  map makeUnsuccessfulParseTest invalidCombinations
  where makeUnsuccessfulParseTest (macron, base, breathing, accent, iotaSub) =
          let input = makeInput macron base breathing accent iotaSub
          in (input ~: assertExn' (\ _ -> return ()) (GP.letter input))

randomWordTests =
  "parsing randomly-generated words" ~:
  testQuickCheck QC.stdArgs propParseCorrect

propParseCorrect :: Word -> Bool
propParseCorrect w =
  CE.run (GP.word (Text.concat (map letterToUnicode (getLetters w))))
  == Right w

makeInput :: Macron -> Char -> Breathing -> Accent -> IotaSub -> String
makeInput macron base breathing accent iotaSub =
  Prelude.concat [
    case macron of
      NoMacron -> ""
      Macron -> "_",
    [base],
    case breathing of
      NoBreathing -> ""
      Smooth -> [combSmooth]
      Rough -> [combRough],
    case accent of
      NoAccent -> ""
      Acute -> [combAcute]
      Grave -> [combGrave]
      Circumflex -> [combCirc],
    case iotaSub of
      NoIotaSub -> ""
      IotaSub -> [combIotaSub]
    ]

errorTests =
  "parsing errors" ~:
  ["empty input (letter)" ~:
   assertExn GP.EmptyInput (GP.letter ""),

   "empty input (word)" ~:
   assertExn GP.EmptyInput (GP.word ""),

   "multiple breath marks" ~:
   assertExn (GP.MultipleBreathing 1 [combSmooth, combRough])
   (GP.word ("ου" ++ [combSmooth, combRough] ++ "τος")),

   "multiple accents" ~:
   assertExn (GP.MultipleAccent 1 [combAcute, combCirc])
   (GP.word ("ου" ++ [combAcute, combCirc] ++ "τος")),

   "invalid macron" ~:
   assertExn (GP.InvalidMacron 2 'ε') (GP.word "αβ_έ"),

   "invalid breathing" ~:
   assertExn (GP.InvalidBreathing 0 'β')
   (GP.word ('β' : combSmooth : "λάπτω")),

   "invalid accent" ~:
   assertExn (GP.InvalidAccent 0 'β')
   (GP.word ('β' : combGrave : "λάπτω")),

   "invalid iota subscript" ~:
   assertExn (GP.InvalidIotaSub 2 'ε')
   (GP.word ("ἀρε" ++ [combIotaSub] ++ "τη")),

   "macron and circumflex" ~:
   assertExn (GP.MacronWithCircumflex 2)
   (GP.word "βλ_ᾶπτω"),

   "macron and iota sub" ~:
   assertExn (GP.MacronWithIotaSub 4)
   (GP.word "ἀγορ_ᾳ"),

   "trailing input" ~:
   assertExn (GP.TrailingInput 1 (normalize "γορ_ά"))
   (GP.letter "ἀγορ_ά"),

   "unsupported character" ~:
   assertExn (GP.UnsupportedChar 2 'x')
   (GP.word "οὐx"),

   "missing letter" ~:
   assertExn (GP.MissingLetter 1)
   (GP.word "β_")]

allCombinations =
  [(macron, base, breathing, accent, iotaSub)
  | macron <- [NoMacron, Macron],
    base <- Set.toList baseChars,
    breathing <- [NoBreathing, Smooth, Rough],
    accent <- [NoAccent, Acute, Grave, Circumflex],
    iotaSub <- [NoIotaSub, IotaSub]]

(validCombinations, invalidCombinations) =
  List.partition valid' allCombinations
  where valid' (macron, base, breathing, accent, iotaSub) =
          validLetter macron base breathing accent iotaSub
