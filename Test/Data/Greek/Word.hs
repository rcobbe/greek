module Test.Data.Greek.Word(tests) where

import Test.HUnit
import qualified Test.QuickCheck as QC
import Test.Utils
import Test.Data.Greek.LetterGen
import Data.Greek.Word

tests =
  "Greek.Word" ~:
  [validityTests, caseTests, caseQCTests]

validityTests = "validity" ~: [
  "bare alpha" ~: validLetter NoMacron 'α' NoBreathing NoAccent NoIotaSub
  ~?= True,
  "bare beta" ~: validLetter NoMacron 'β' NoBreathing NoAccent NoIotaSub
  ~?= True,
  "bare rho" ~: validLetter NoMacron 'ρ' NoBreathing NoAccent NoIotaSub
  ~?= True,
  "rough rho" ~: validLetter NoMacron 'ρ' Rough NoAccent NoIotaSub ~?= True,
  "alpha: smooth acute iota sub" ~:
  validLetter NoMacron 'α' Smooth Acute IotaSub ~?= True,
  "alpha: long circumflex" ~:
  validLetter Macron 'α' NoBreathing Circumflex NoIotaSub ~?= False,
  "alpha: long iota sub" ~:
  validLetter Macron 'α' NoBreathing NoAccent IotaSub ~?= False,
  "circumflex epsilon" ~:
  validLetter NoMacron 'ε' NoBreathing Circumflex NoIotaSub ~?= False,
  "long epsilon" ~:
  validLetter Macron 'ε' NoBreathing NoAccent NoIotaSub ~?= False,
  "epsilon iota-sub" ~:
  validLetter NoMacron 'ε' NoBreathing NoAccent IotaSub ~?= False,
  "rho acute" ~:
  validLetter NoMacron 'ρ' NoBreathing Acute IotaSub ~?= False]

caseTests = "case functions" ~: [
  "isUpper of capital" ~: isUpper capTheta ~?= True,

  "isUpper of lowercase" ~: isUpper lowercaseTheta ~?= False,

  "isLower of capital" ~: isLower capTheta ~?= False,

  "isLower of lowercase" ~: isLower lowercaseTheta ~?= True,

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
