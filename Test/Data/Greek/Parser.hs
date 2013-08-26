module Test.Data.Greek.Parser(tests) where

import qualified Data.Text.Lazy as Text
import qualified Data.Set as Set
import Test.HUnit

import Text.Parsec.Error as PError

import Test.Utils
import Test.Data.Greek.Utils
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
   (Right letter) ~?= GP.letter (letterToUnicode letter)
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
   GP.letter [baseEpsilon, combIotaSub]
   ~?= Left (GP.InvalidInputException [combIotaSub]),

   "trailing input" ~:
   let input = "ᾄγ"
   in GP.letter input
      ~?= Left (GP.InvalidInputException (normalize input))
  ]

wordTests =
  "parsing complete words" ~:
  ["standalone word" ~:
   GP.word "ἀδελφός"
   ~?=
   Right (makeWord [makeLetter 'α' Smooth NoAccent NoIotaSub NoMacron,
                    makeLetter 'δ' NoBreathing NoAccent NoIotaSub NoMacron,
                    makeLetter 'ε' NoBreathing NoAccent NoIotaSub NoMacron,
                    makeLetter 'λ' NoBreathing NoAccent NoIotaSub NoMacron,
                    makeLetter 'φ' NoBreathing NoAccent NoIotaSub NoMacron,
                    makeLetter 'ο' NoBreathing Acute NoIotaSub NoMacron,
                    makeLetter 'ς' NoBreathing NoAccent NoIotaSub NoMacron]),

   "word with following whitespace" ~:
   Left (GP.InvalidInputException "\n")
   ~?= GP.word "ἀγορ_ά\n",

   "greek text with following punctuation" ~:
   Left (GP.InvalidInputException ")")
   ~?= GP.word "οὐ)",

   "greek text with trailing non-greek characters" ~:
   Left (GP.InvalidInputException "k")
   ~?= GP.word "οὐk"]

errorTests =
  let circError =
        (PError.Message "circumflex and macron may not occur together")
  in
   "customized parsing errors" ~:
  ["alpha with macron and circumflex" ~:
   GP.letter "_ᾶ"
   ~?= Left (GP.InvalidInputException [combCirc]),

   "alpha with macron and iota subscript" ~:
   GP.letter "_ᾳ"
   ~?= Left (GP.InvalidInputException [combIotaSub]),

   "iota with macron & circumflex" ~:
   GP.letter "_ῖ"
   ~?= Left (GP.InvalidInputException [combCirc]),

   "upsilon with macron and circumflex" ~:
   GP.letter "_ῦ"
   ~?= Left (GP.InvalidInputException [combCirc])]
