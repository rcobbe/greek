module Test.Data.Greek.Word(tests) where

import Test.HUnit
import Data.Greek.Word

tests =
  "Greek.Word" ~:
  ["validity" ~: validityTests]

validityTests =
  ["bare alpha" ~: validLetter NoMacron 'α' NoBreathing NoAccent NoIotaSub
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
