module Test.Data.Greek.Parser(tests) where

import Control.Exceptional.HUnit
import qualified Data.Set as Set

import Test.HUnit

import Data.Greek.Word
import qualified Data.Greek.Parser as GP
import Data.Greek.Output
import Data.Greek.Normalize
import Data.Greek.UnicodeData

-- XXX remove explicit calls to normalize when we finish overhauling parser.

tests =
  "Greek.Parser" ~:
  [allLetterTests, otherLetterTests, wordTests, errorTests]

allLetterTests =
  "parsing individual letters" ~:
  [show letter ~:
   assertNoExn letter (GP.letter (letterToUnicode letter))
   | base <- Set.toList baseChars,
     breathing <- [NoBreathing, Smooth, Rough],
     accent <- [NoAccent, Acute, Grave, Circumflex],
     iotaSub <- [NoIotaSub, IotaSub],
     macron <- [NoMacron, Macron],
     validLetter macron base breathing accent iotaSub,
     let letter = makeLetter base breathing accent iotaSub macron]

otherLetterTests =
  "other tests involving parsing letters" ~:
  ["spurious diacritical" ~:
   assertExn' (\ _ -> return ()) (GP.letter [baseEpsilon, combIotaSub]),

   "trailing input" ~:
   assertNoExn (makeLetter baseAlpha Smooth Acute IotaSub NoMacron)
     (GP.letter (normalize "ᾄγ"))
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
     (GP.word (normalize "ἀδελφός")),

   "word with following whitespace" ~:
   assertNoExn
     (makeWord [makeLetter baseAlpha Smooth NoAccent NoIotaSub NoMacron,
                makeLetter baseGamma NoBreathing NoAccent NoIotaSub NoMacron,
                makeLetter baseOmicron NoBreathing NoAccent NoIotaSub NoMacron,
                makeLetter baseRho NoBreathing NoAccent NoIotaSub NoMacron,
                makeLetter baseAlpha NoBreathing Acute NoIotaSub Macron])
     (GP.word (normalize "ἀγορ_ά\n")),

   "greek text with following punctuation" ~:
   assertNoExn
     (makeWord [makeLetter baseOmicron NoBreathing NoAccent NoIotaSub NoMacron,
                makeLetter baseUpsilon Smooth NoAccent NoIotaSub NoMacron])
     (GP.word (normalize "οὐ)")),

   "greek text with trailing non-greek characters" ~:
   assertNoExn
     (makeWord [makeLetter baseOmicron NoBreathing NoAccent NoIotaSub NoMacron,
                makeLetter baseUpsilon Smooth NoAccent NoIotaSub NoMacron])
     (GP.word (normalize "οὐk"))]

errorTests =
  let greekLetter = "greek letter"
      circError = "circumflex and macron may not occur together"
  in
   "customized parsing errors" ~:
  ["alpha with macron and circumflex" ~:
   assertExn (GP.ParseError 1 [greekLetter, circError])
     (GP.letter (normalize "_ᾶ")),

   "alpha with macron and iota subscript" ~:
   assertExn
     (GP.ParseError 1 [greekLetter,
                       "iota subscript and macron may not occur together"])
     (GP.letter (normalize "_ᾳ")),

   "iota with macron & circumflex" ~:
   assertExn (GP.ParseError 1 [greekLetter, circError])
     (GP.letter (normalize "_ῖ")),

   "upsilon with macron and circumflex" ~:
   assertExn (GP.ParseError 1 [greekLetter, circError])
     (GP.letter (normalize "_ῦ"))]
