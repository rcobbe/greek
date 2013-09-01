module Test.Data.Greek.Parser(tests) where

import Control.Exceptional.HUnit
import qualified Data.Set as Set

import Test.HUnit

import Data.Greek.Word
import qualified Data.Greek.Parser as GP
import Data.Greek.Output
import Data.Greek.Normalize
import Data.Greek.UnicodeData

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
     (GP.word "ἀγορ_ά\n"),

   "greek text with following punctuation" ~:
   assertNoExn
     (makeWord [makeLetter baseOmicron NoBreathing NoAccent NoIotaSub NoMacron,
                makeLetter baseUpsilon Smooth NoAccent NoIotaSub NoMacron])
     (GP.word "οὐ)"),

   "greek text with trailing non-greek characters" ~:
   assertNormalizedParse (GP.literalWord "οὐ", "k") (parseRest GP.word) "οὐk"]

errorTests =
  let circError =
        (PError.Message "circumflex and macron may not occur together")
  in
   "customized parsing errors" ~:
  ["alpha with macron and circumflex" ~:
   assertParseError GP.letter (normalize "_ᾶ") circError,

   "alpha with macron and iota subscript" ~:
   assertParseError GP.letter (normalize "_ᾳ")
   (PError.Message "iota subscript and macron may not occur together"),

   "iota with macron & circumflex" ~:
   assertParseError GP.letter (normalize "_ῖ") circError,

   "upsilon with macron and circumflex" ~:
   assertParseError GP.letter (normalize "_ῦ") circError]
