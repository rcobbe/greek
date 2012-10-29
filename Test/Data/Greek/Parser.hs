{-# LANGUAGE OverloadedStrings #-}

module Test.Data.Greek.Parser(tests) where

import qualified Data.Text.Lazy as Text
import qualified Data.Set as Set
import Test.HUnit

import Test.Utils
import Test.Data.Greek.Utils
import Data.Greek.Word
import qualified Data.Greek.Parser as GP
import Data.Greek.Output
import Data.Greek.Normalize
import Data.Greek.UnicodeData

tests =
  "Greek.Parser" ~:
  [allLetterTests, otherLetterTests, wordTests]

allLetterTests =
  "parsing individual letters" ~:
  [show letter ~:
   assertNormalizedParse letter GP.letter (letterToUnicode letter)
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
    assertParseFail GP.letter (Text.pack [baseEpsilon, combIotaSub]),

   "trailing input" ~:
   assertNormalizedParse ((makeLetter baseAlpha Smooth Acute IotaSub NoMacron),
                          "γ")
   (parseRest GP.letter) (normalize "ᾄγ")
  ]

wordTests =
  "parsing complete words" ~:
  ["standalone word" ~:
   assertNormalizedParse (GP.literalWord "ἀδελφός", "") (parseRest GP.word)
   "ἀδελφός",

   "word with following whitespace" ~:
   assertNormalizedParse (GP.literalWord "ἀγορ_ά", "\n")
   (parseRest GP.word) "ἀγορ_ά\n",

   "greek text with following punctuation" ~:
   assertNormalizedParse (GP.literalWord "οὐ", ")") (parseRest GP.word) "οὐ)",

   "greek text with trailing non-greek characters" ~:
   assertNormalizedParse (GP.literalWord "οὐ", "k") (parseRest GP.word) "οὐk"]
