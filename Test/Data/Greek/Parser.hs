module Test.Data.Greek.Parser(tests) where

import Control.Exceptional.HUnit
import qualified Data.Set as Set

import Test.HUnit

import Data.Greek.Word
import qualified Data.Greek.Parser as GP
import Data.Greek.Output
import Data.Greek.UnicodeData

tests =
  "Greek.Parser" ~:
  [allLetterTests, otherLetterTests, wordTests, errorTests]

allLetterTests =
  "parsing individual letters" ~:
  [show letter ~:
   assertNoExn letter (GP.letter ((letterToUnicode letter) :: String))
   | base <- Set.toList baseChars,
     breathing <- [NoBreathing, Smooth, Rough],
     accent <- [NoAccent, Acute, Grave, Circumflex],
     iotaSub <- [NoIotaSub, IotaSub],
     macron <- [NoMacron, Macron],
     validLetter macron base breathing accent iotaSub,
     let letter = makeLetter base breathing accent iotaSub macron]

-- XXX replace these with more detailed tests.
otherLetterTests =
  "other tests involving parsing letters" ~:
  ["spurious diacritical" ~:
   assertExn' (\ _ -> return ()) (GP.letter [baseEpsilon, combIotaSub]),

   "trailing input" ~:
   assertExn (GP.TrailingInput 1 "γ")
     (GP.letter "ᾄγ")
  ]

wordTests =
  "parsing complete words" ~:
  ["standalone word" ~:
   assertNoExn
     (makeWord [makeLetter baseAlpha Smooth NoAccent NoIotaSub NoMacron,
                makeLetter baseDelta NoBreathing NoAccent NoIotaSub NoMacron,
                makeLetter baseEpsilon NoBreathing NoAccent NoIotaSub NoMacron,
                makeLetter baseLambda NoBreathing NoAccent NoIotaSub NoMacron,
                makeLetter basePhi NoBreathing NoAccent NoIotaSub NoMacron,
                makeLetter baseOmicron NoBreathing Acute NoIotaSub NoMacron,
                makeLetter baseFinalSigma
                  NoBreathing NoAccent NoIotaSub NoMacron])
     (GP.word "ἀδελφός"),

   "word with following whitespace" ~:
   assertExn (GP.TrailingInput 5 "\n")
     (GP.word "ἀγορ_ά\n"),

   "greek text with following punctuation" ~:
   assertExn
     (GP.TrailingInput 2 ")")
     (GP.word "οὐ)"),

   "greek text with trailing non-greek characters" ~:
   assertExn
     (GP.TrailingInput 2 "k")
     (GP.word "οὐk")]

-- XXX improve error-case test coverage.

errorTests = "customized parsing errors" ~:
  ["alpha with macron and circumflex" ~:
   assertExn
     (GP.MacronWithCircumflex 0)
     (GP.letter "_ᾶ"),

   "alpha with macron and iota subscript" ~:
   assertExn
     (GP.MacronWithIotaSub 0)
     (GP.letter "_ᾳ"),

   "iota with macron & circumflex" ~:
   assertExn
     (GP.MacronWithCircumflex 0)
     (GP.letter "_ῖ"),

   "upsilon with macron and circumflex" ~:
   assertExn
     (GP.MacronWithCircumflex 0)
     (GP.letter "_ῦ")]
