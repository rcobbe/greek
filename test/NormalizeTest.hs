{-# LANGUAGE OverloadedStrings #-}

-- Copyright 2012-2017 Richard Cobbe
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--   http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.

module NormalizeTest(tests) where

import Data.List (foldl')
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text.Lazy (Text, pack)
import qualified Data.Text.Lazy as Text
import Test.HUnit
import Data.Greek.Normalize
import Data.Greek.UnicodeData

tests =
  "Greek.Normalize" ~:
  [sanityTests,
   singleCharTests,
   stringTests]

sanityTests =
  "test definitions" ~:
  ["\"testSpecs\" defines a function" ~:
   findDuplicate (map fst testSpecs) ~?= Nothing,

   "dom(testSpecs) includes all necessary elements" ~:
   Set.difference expectedDomain actualDomain ~?= Set.empty,

   "dom(testSpecs) includes no extra elements" ~:
   Set.difference actualDomain expectedDomain ~?= Set.empty]

singleCharTests =
  "single-character normalization" ~:
  map mkTest testSpecs
  where mkTest (precomposedChar, expectedString) =
          [precomposedChar] ~:
            normalize (Text.singleton precomposedChar) ~?= expectedString

stringTests =
  "string normalization" ~:
  ["simple ascii" ~: normalize ("abc" :: Text) ~?= "abc",

   "simple greek" ~: normalize ("αγορα" :: Text) ~?= "αγορα",

   "greek with combining accents" ~:
   normalize (pack [baseEta, combIotaSub, combCirc, combSmooth, baseNu])
   ~?= pack ([baseEta, combSmooth, combCirc, combIotaSub, baseNu]),

   "compound base chars, no combining characters" ~:
   normalize (pack [alphaSmooth, baseGamma, baseOmicron, baseRho, alphaAcute])
   ~?=
   pack [baseAlpha, combSmooth, baseGamma, baseOmicron, baseRho, baseAlpha,
         combAcute],

   "compound base chars with combining accents" ~:
   normalize (pack [etaCircIotaSub, combSmooth, baseNu]) ~?=
   pack [baseEta, combSmooth, combCirc, combIotaSub, baseNu],

   "combining sequence interrupted by out-of-range character" ~:
   normalize (pack [baseEta, combIotaSub, combCirc, 'x', combAcute, combSmooth,
                    baseNu])
   ~?= pack [baseEta, combCirc, combIotaSub, 'x', combAcute, combSmooth,
             baseNu],

   "underscore plus compound base chars" ~:
   normalize (pack [alphaSmooth, baseGamma, baseOmicron, baseRho, '_',
                    alphaAcute])
   ~?= pack [baseAlpha, combSmooth, baseGamma, baseOmicron, baseRho, '_',
             baseAlpha, combAcute],

   "underscore plus combining accents" ~:
   normalize (pack [baseAlpha, combSmooth, baseGamma, baseOmicron, baseRho, '_',
                    baseAlpha, combIotaSub, combAcute])
   ~?= pack [baseAlpha, combSmooth, baseGamma, baseOmicron, baseRho, '_',
             baseAlpha, combAcute, combIotaSub]]


-- | Finds the first duplicate element in the input list; returns 'Nothing' if
--   no duplicates.
findDuplicate :: Ord a => [a] -> Maybe a
findDuplicate elems =
  fst (foldl checkElem (Nothing, Set.empty) elems)
  where checkElem :: Ord a => (Maybe a, Set a) -> a -> (Maybe a, Set a)
        checkElem accum@(Just _, _) _ = accum
        checkElem (Nothing, s) elem
          | elem `Set.member` s = (Just elem, s)
          | otherwise = (Nothing, Set.insert elem s)

testSpecs :: [(Char, Text)]
testSpecs =
  map (\ (c, str) -> (c, Text.pack str))
  [(capIotaDialytika, [capIota, combDialytika]),
   (capUpsilonDialytika, [capUpsilon, combDialytika]),
   (iotaDialytika, [baseIota, combDialytika]),
   (upsilonDialytika, [baseUpsilon, combDialytika]),
   (alphaSmooth, [baseAlpha, combSmooth]),
   (alphaRough, [baseAlpha, combRough]),
   (alphaSmoothGrave, [baseAlpha, combSmooth, combGrave]),
   (alphaSmoothAcute, [baseAlpha, combSmooth, combAcute]),
   (alphaRoughGrave, [baseAlpha, combRough, combGrave]),
   (alphaRoughAcute, [baseAlpha, combRough, combAcute]),
   (alphaSmoothCirc, [baseAlpha, combSmooth, combCirc]),
   (alphaRoughCirc, [baseAlpha, combRough, combCirc]),
   (capAlphaSmooth, [capAlpha, combSmooth]),
   (capAlphaRough, [capAlpha, combRough]),
   (capAlphaSmoothGrave, [capAlpha, combSmooth, combGrave]),
   (capAlphaRoughGrave, [capAlpha, combRough, combGrave]),
   (capAlphaSmoothAcute, [capAlpha, combSmooth, combAcute]),
   (capAlphaRoughAcute, [capAlpha, combRough, combAcute]),
   (capAlphaSmoothCirc, [capAlpha, combSmooth, combCirc]),
   (capAlphaRoughCirc, [capAlpha, combRough, combCirc]),

   (epsilonSmooth, [baseEpsilon, combSmooth]),
   (epsilonRough, [baseEpsilon, combRough]),
   (epsilonSmoothGrave, [baseEpsilon, combSmooth, combGrave]),
   (epsilonSmoothAcute, [baseEpsilon, combSmooth, combAcute]),
   (epsilonRoughGrave, [baseEpsilon, combRough, combGrave]),
   (epsilonRoughAcute, [baseEpsilon, combRough, combAcute]),
   (capEpsilonSmooth, [capEpsilon, combSmooth]),
   (capEpsilonRough, [capEpsilon, combRough]),
   (capEpsilonSmoothGrave, [capEpsilon, combSmooth, combGrave]),
   (capEpsilonRoughGrave, [capEpsilon, combRough, combGrave]),
   (capEpsilonSmoothAcute, [capEpsilon, combSmooth, combAcute]),
   (capEpsilonRoughAcute, [capEpsilon, combRough, combAcute]),

   (etaSmooth, [baseEta, combSmooth]),
   (etaRough, [baseEta, combRough]),
   (etaSmoothGrave, [baseEta, combSmooth, combGrave]),
   (etaSmoothAcute, [baseEta, combSmooth, combAcute]),
   (etaRoughGrave, [baseEta, combRough, combGrave]),
   (etaRoughAcute, [baseEta, combRough, combAcute]),
   (etaSmoothCirc, [baseEta, combSmooth, combCirc]),
   (etaRoughCirc, [baseEta, combRough, combCirc]),
   (capEtaSmooth, [capEta, combSmooth]),
   (capEtaRough, [capEta, combRough]),
   (capEtaSmoothGrave, [capEta, combSmooth, combGrave]),
   (capEtaRoughGrave, [capEta, combRough, combGrave]),
   (capEtaSmoothAcute, [capEta, combSmooth, combAcute]),
   (capEtaRoughAcute, [capEta, combRough, combAcute]),
   (capEtaSmoothCirc, [capEta, combSmooth, combCirc]),
   (capEtaRoughCirc, [capEta, combRough, combCirc]),

   (iotaSmooth, [baseIota, combSmooth]),
   (iotaRough, [baseIota, combRough]),
   (iotaSmoothGrave, [baseIota, combSmooth, combGrave]),
   (iotaSmoothAcute, [baseIota, combSmooth, combAcute]),
   (iotaRoughGrave, [baseIota, combRough, combGrave]),
   (iotaRoughAcute, [baseIota, combRough, combAcute]),
   (iotaSmoothCirc, [baseIota, combSmooth, combCirc]),
   (iotaRoughCirc, [baseIota, combRough, combCirc]),
   (capIotaSmooth, [capIota, combSmooth]),
   (capIotaRough, [capIota, combRough]),
   (capIotaSmoothGrave, [capIota, combSmooth, combGrave]),
   (capIotaRoughGrave, [capIota, combRough, combGrave]),
   (capIotaSmoothAcute, [capIota, combSmooth, combAcute]),
   (capIotaRoughAcute, [capIota, combRough, combAcute]),
   (capIotaSmoothCirc, [capIota, combSmooth, combCirc]),
   (capIotaRoughCirc, [capIota, combRough, combCirc]),

   (omicronSmooth, [baseOmicron, combSmooth]),
   (omicronRough, [baseOmicron, combRough]),
   (omicronSmoothGrave, [baseOmicron, combSmooth, combGrave]),
   (omicronSmoothAcute, [baseOmicron, combSmooth, combAcute]),
   (omicronRoughGrave, [baseOmicron, combRough, combGrave]),
   (omicronRoughAcute, [baseOmicron, combRough, combAcute]),
   (capOmicronSmooth, [capOmicron, combSmooth]),
   (capOmicronRough, [capOmicron, combRough]),
   (capOmicronSmoothGrave, [capOmicron, combSmooth, combGrave]),
   (capOmicronRoughGrave, [capOmicron, combRough, combGrave]),
   (capOmicronSmoothAcute, [capOmicron, combSmooth, combAcute]),
   (capOmicronRoughAcute, [capOmicron, combRough, combAcute]),

   (upsilonSmooth, [baseUpsilon, combSmooth]),
   (upsilonRough, [baseUpsilon, combRough]),
   (upsilonSmoothGrave, [baseUpsilon, combSmooth, combGrave]),
   (upsilonSmoothAcute, [baseUpsilon, combSmooth, combAcute]),
   (upsilonRoughGrave, [baseUpsilon, combRough, combGrave]),
   (upsilonRoughAcute, [baseUpsilon, combRough, combAcute]),
   (upsilonSmoothCirc, [baseUpsilon, combSmooth, combCirc]),
   (upsilonRoughCirc, [baseUpsilon, combRough, combCirc]),
   (capUpsilonRough, [capUpsilon, combRough]),
   (capUpsilonRoughGrave, [capUpsilon, combRough, combGrave]),
   (capUpsilonRoughAcute, [capUpsilon, combRough, combAcute]),
   (capUpsilonRoughCirc, [capUpsilon, combRough, combCirc]),

   (omegaSmooth, [baseOmega, combSmooth]),
   (omegaRough, [baseOmega, combRough]),
   (omegaSmoothGrave, [baseOmega, combSmooth, combGrave]),
   (omegaSmoothAcute, [baseOmega, combSmooth, combAcute]),
   (omegaRoughGrave, [baseOmega, combRough, combGrave]),
   (omegaRoughAcute, [baseOmega, combRough, combAcute]),
   (omegaSmoothCirc, [baseOmega, combSmooth, combCirc]),
   (omegaRoughCirc, [baseOmega, combRough, combCirc]),
   (capOmegaSmooth, [capOmega, combSmooth]),
   (capOmegaRough, [capOmega, combRough]),
   (capOmegaSmoothGrave, [capOmega, combSmooth, combGrave]),
   (capOmegaSmoothAcute, [capOmega, combSmooth, combAcute]),
   (capOmegaRoughGrave, [capOmega, combRough, combGrave]),
   (capOmegaRoughAcute, [capOmega, combRough, combAcute]),
   (capOmegaSmoothCirc, [capOmega, combSmooth, combCirc]),
   (capOmegaRoughCirc, [capOmega, combRough, combCirc]),

   (alphaGrave, [baseAlpha, combGrave]),
   (alphaAcute, [baseAlpha, combAcute]),
   (epsilonGrave, [baseEpsilon, combGrave]),
   (epsilonAcute, [baseEpsilon, combAcute]),
   (etaGrave, [baseEta, combGrave]),
   (etaAcute, [baseEta, combAcute]),
   (iotaGrave, [baseIota, combGrave]),
   (iotaAcute, [baseIota, combAcute]),
   (omicronGrave, [baseOmicron, combGrave]),
   (omicronAcute, [baseOmicron, combAcute]),
   (upsilonGrave, [baseUpsilon, combGrave]),
   (upsilonAcute, [baseUpsilon, combAcute]),
   (omegaGrave, [baseOmega, combGrave]),
   (omegaAcute, [baseOmega, combAcute]),

   (alphaSmoothIotaSub, [baseAlpha, combSmooth, combIotaSub]),
   (alphaRoughIotaSub, [baseAlpha, combRough, combIotaSub]),
   (alphaSmoothGraveIotaSub, [baseAlpha, combSmooth, combGrave, combIotaSub]),
   (alphaRoughGraveIotaSub, [baseAlpha, combRough, combGrave, combIotaSub]),
   (alphaSmoothAcuteIotaSub, [baseAlpha, combSmooth, combAcute, combIotaSub]),
   (alphaRoughAcuteIotaSub, [baseAlpha, combRough, combAcute, combIotaSub]),
   (alphaSmoothCircIotaSub, [baseAlpha, combSmooth, combCirc, combIotaSub]),
   (alphaRoughCircIotaSub, [baseAlpha, combRough, combCirc, combIotaSub]),
   (capAlphaSmoothIotaSub, [capAlpha, combSmooth, combIotaSub]),
   (capAlphaRoughIotaSub, [capAlpha, combRough, combIotaSub]),
   (capAlphaSmoothGraveIotaSub, [capAlpha, combSmooth, combGrave, combIotaSub]),
   (capAlphaRoughGraveIotaSub, [capAlpha, combRough, combGrave, combIotaSub]),
   (capAlphaSmoothAcuteIotaSub, [capAlpha, combSmooth, combAcute, combIotaSub]),
   (capAlphaRoughAcuteIotaSub, [capAlpha, combRough, combAcute, combIotaSub]),
   (capAlphaSmoothCircIotaSub, [capAlpha, combSmooth, combCirc, combIotaSub]),
   (capAlphaRoughCircIotaSub, [capAlpha, combRough, combCirc, combIotaSub]),

   (etaSmoothIotaSub, [baseEta, combSmooth, combIotaSub]),
   (etaRoughIotaSub, [baseEta, combRough, combIotaSub]),
   (etaSmoothGraveIotaSub, [baseEta, combSmooth, combGrave, combIotaSub]),
   (etaRoughGraveIotaSub, [baseEta, combRough, combGrave, combIotaSub]),
   (etaSmoothAcuteIotaSub, [baseEta, combSmooth, combAcute, combIotaSub]),
   (etaRoughAcuteIotaSub, [baseEta, combRough, combAcute, combIotaSub]),
   (etaSmoothCircIotaSub, [baseEta, combSmooth, combCirc, combIotaSub]),
   (etaRoughCircIotaSub, [baseEta, combRough, combCirc, combIotaSub]),
   (capEtaSmoothIotaSub, [capEta, combSmooth, combIotaSub]),
   (capEtaRoughIotaSub, [capEta, combRough, combIotaSub]),
   (capEtaSmoothGraveIotaSub, [capEta, combSmooth, combGrave, combIotaSub]),
   (capEtaRoughGraveIotaSub, [capEta, combRough, combGrave, combIotaSub]),
   (capEtaSmoothAcuteIotaSub, [capEta, combSmooth, combAcute, combIotaSub]),
   (capEtaRoughAcuteIotaSub, [capEta, combRough, combAcute, combIotaSub]),
   (capEtaSmoothCircIotaSub, [capEta, combSmooth, combCirc, combIotaSub]),
   (capEtaRoughCircIotaSub, [capEta, combRough, combCirc, combIotaSub]),

   (omegaSmoothIotaSub, [baseOmega, combSmooth, combIotaSub]),
   (omegaRoughIotaSub, [baseOmega, combRough, combIotaSub]),
   (omegaSmoothGraveIotaSub, [baseOmega, combSmooth, combGrave, combIotaSub]),
   (omegaRoughGraveIotaSub, [baseOmega, combRough, combGrave, combIotaSub]),
   (omegaSmoothAcuteIotaSub, [baseOmega, combSmooth, combAcute, combIotaSub]),
   (omegaRoughAcuteIotaSub, [baseOmega, combRough, combAcute, combIotaSub]),
   (omegaSmoothCircIotaSub, [baseOmega, combSmooth, combCirc, combIotaSub]),
   (omegaRoughCircIotaSub, [baseOmega, combRough, combCirc, combIotaSub]),
   (capOmegaSmoothIotaSub, [capOmega, combSmooth, combIotaSub]),
   (capOmegaRoughIotaSub, [capOmega, combRough, combIotaSub]),
   (capOmegaSmoothGraveIotaSub, [capOmega, combSmooth, combGrave, combIotaSub]),
   (capOmegaRoughGraveIotaSub, [capOmega, combRough, combGrave, combIotaSub]),
   (capOmegaSmoothAcuteIotaSub, [capOmega, combSmooth, combAcute, combIotaSub]),
   (capOmegaRoughAcuteIotaSub, [capOmega, combRough, combAcute, combIotaSub]),
   (capOmegaSmoothCircIotaSub, [capOmega, combSmooth, combCirc, combIotaSub]),
   (capOmegaRoughCircIotaSub, [capOmega, combRough, combCirc, combIotaSub]),

   (alphaGraveIotaSub, [baseAlpha, combGrave, combIotaSub]),
   (alphaIotaSub, [baseAlpha, combIotaSub]),
   (alphaAcuteIotaSub, [baseAlpha, combAcute, combIotaSub]),
   (alphaCirc, [baseAlpha, combCirc]),
   (alphaCircIotaSub, [baseAlpha, combCirc, combIotaSub]),
   -- why do we have capital vowels without breathing marks but cap vowels +
   -- iota subscript only with breathing marks?
   (capAlphaGrave, [capAlpha, combGrave]),
   (capAlphaAcute, [capAlpha, combAcute]),
   (capAlphaIotaSub, [capAlpha, combIotaSub]),

   (etaGraveIotaSub, [baseEta, combGrave, combIotaSub]),
   (etaIotaSub, [baseEta, combIotaSub]),
   (etaAcuteIotaSub, [baseEta, combAcute, combIotaSub]),
   (etaCirc, [baseEta, combCirc]),
   (etaCircIotaSub, [baseEta, combCirc, combIotaSub]),
   (capEpsilonGrave, [capEpsilon, combGrave]),
   (capEpsilonAcute, [capEpsilon, combAcute]),
   (capEtaGrave, [capEta, combGrave]),
   (capEtaAcute, [capEta, combAcute]),
   (capEtaIotaSub, [capEta, combIotaSub]),

   (iotaGraveDialytika, [baseIota, combDialytika, combGrave]),
   (iotaAcuteDialytika, [baseIota, combDialytika, combAcute]),
   (iotaCirc, [baseIota, combCirc]),
   (iotaCircDialytika, [baseIota, combDialytika, combCirc]),
   (capIotaGrave, [capIota, combGrave]),
   (capIotaAcute, [capIota, combAcute]),

   (upsilonGraveDialytika, [baseUpsilon, combDialytika, combGrave]),
   (upsilonAcuteDialytika, [baseUpsilon, combDialytika, combAcute]),
   (rhoSmooth, [baseRho, combSmooth]),
   (rhoRough, [baseRho, combRough]),
   (upsilonCirc, [baseUpsilon, combCirc]),
   (upsilonCircDialytika, [baseUpsilon, combDialytika, combCirc]),
   (capUpsilonGrave, [capUpsilon, combGrave]),
   (capUpsilonAcute, [capUpsilon, combAcute]),
   (capRhoRough, [capRho, combRough]),

   (omegaGraveIotaSub, [baseOmega, combGrave, combIotaSub]),
   (omegaIotaSub, [baseOmega, combIotaSub]),
   (omegaAcuteIotaSub, [baseOmega, combAcute, combIotaSub]),
   (omegaCirc, [baseOmega, combCirc]),
   (omegaCircIotaSub, [baseOmega, combCirc, combIotaSub]),
   (capOmicronGrave, [capOmicron, combGrave]),
   (capOmicronAcute, [capOmicron, combAcute]),
   (capOmegaGrave, [capOmega, combGrave]),
   (capOmegaAcute, [capOmega, combAcute]),
   (capOmegaIotaSub, [capOmega, combIotaSub])]

-- Expected domain for 'testSpecs'
expectedDomain :: Set Char
expectedDomain =
  Set.union greekExtendedChars (Set.fromList
                                [capIotaDialytika, capUpsilonDialytika,
                                 iotaDialytika, upsilonDialytika])

-- Actual domain of 'testSpecs'
actualDomain :: Set Char
actualDomain = Set.fromList (map fst testSpecs)
