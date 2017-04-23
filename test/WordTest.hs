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

module WordTest(tests) where

import Test.HUnit
import qualified Test.QuickCheck as QC
import Test.Utils
import GreekLetterGen
import Data.Greek.Word

tests =
  "Greek.Word" ~:
  [validityTests, caseTests, caseQCTests]

validityTests = "validity" ~: [
  "bare alpha" ~:
    assert (validLetter NoMacron 'α' NoBreathing NoAccent NoIotaSub),
  "bare beta" ~:
    assert (validLetter NoMacron 'β' NoBreathing NoAccent NoIotaSub),
  "bare rho" ~:
    assert (validLetter NoMacron 'ρ' NoBreathing NoAccent NoIotaSub),
  "rough rho" ~:
    assert (validLetter NoMacron 'ρ' Rough NoAccent NoIotaSub),
  "alpha: smooth acute iota sub" ~:
    assert (validLetter NoMacron 'α' Smooth Acute IotaSub),
  "alpha: long circumflex" ~:
    assertFalse (validLetter Macron 'α' NoBreathing Circumflex NoIotaSub),
  "alpha: long iota sub" ~:
    assertFalse (validLetter Macron 'α' NoBreathing NoAccent IotaSub),
  "circumflex epsilon" ~:
    assertFalse (validLetter NoMacron 'ε' NoBreathing Circumflex NoIotaSub),
  "long epsilon" ~:
    assertFalse (validLetter Macron 'ε' NoBreathing NoAccent NoIotaSub),
  "epsilon iota-sub" ~:
    assertFalse (validLetter NoMacron 'ε' NoBreathing NoAccent IotaSub),
  "rho acute" ~:
    assertFalse (validLetter NoMacron 'ρ' NoBreathing Acute IotaSub)]

caseTests = "case functions" ~: [
  "isUpper of capital" ~: assert (isUpper capTheta),

  "isUpper of lowercase" ~: assertFalse (isUpper lowercaseTheta),

  "isLower of capital" ~: assertFalse (isLower capTheta),

  "isLower of lowercase" ~: assert (isLower lowercaseTheta),

  "toUpper of capital" ~: toUpper capTheta ~?= capTheta,

  "toUpper of lowercase" ~: toUpper lowercaseTheta ~?= capTheta,

  "toLower of capital" ~: toLower capTheta ~?= lowercaseTheta,

  "toLower of lowercase" ~: toLower lowercaseTheta ~?= lowercaseTheta,

  "toUpper, final sigma" ~: toUpper finalSigma ~?= capSigma,

  "toUpper, medial sigma" ~: toUpper medialSigma ~?= capSigma,

  "toLower, capSigma" ~: toLower capSigma ~?= medialSigma]

caseQCTests =
  "case functions preserve other properties" ~: [
    "toUpper" ~:
    testQuickCheck (QC.stdArgs { QC.maxSuccess = 1000 })
    propToUpperPreserves,

    "toLower" ~:
    testQuickCheck (QC.stdArgs { QC.maxSuccess = 1000 })
    propToLowerPreserves]

propToUpperPreserves :: Letter -> Bool
propToUpperPreserves l =
  let upper = toUpper l
  in getMacron l == getMacron upper
     && getBreathing l == getBreathing upper
     && getAccent l == getAccent upper
     && getIotaSub l == getIotaSub upper

propToLowerPreserves :: Letter -> Bool
propToLowerPreserves l =
  let lower = toLower l
  in getMacron l == getMacron lower
     && getBreathing l == getBreathing lower
     && getAccent l == getAccent lower
     && getIotaSub l == getIotaSub lower

capTheta = makeLetter 'Θ' NoBreathing NoAccent NoIotaSub NoMacron
lowercaseTheta = makeLetter 'θ' NoBreathing NoAccent NoIotaSub NoMacron
capSigma = makeLetter 'Σ' NoBreathing NoAccent NoIotaSub NoMacron
medialSigma = makeLetter 'σ' NoBreathing NoAccent NoIotaSub NoMacron
finalSigma = makeLetter 'ς' NoBreathing NoAccent NoIotaSub NoMacron

assertFalse = assert . not
