{-# LANGUAGE OverloadedStrings #-}

module OutputTest(tests) where

import Prelude hiding (Word)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as Text
import Test.HUnit
import Data.Greek.Output
import Data.Greek.Normalize
import Data.Greek.Word

-- We test the conversion to Unicode explicitly so we can use it for the parser
-- tests.

tests =
  "Greek.Output" ~:
  ["to Unicode" ~: makeUnicodeTests addTextMacron letterToUnicode wordToUnicode,
   "to LaTeX" ~: makeUnicodeTests addLaTeXMacron letterToLaTeX wordToLaTeX]

addTextMacron :: Text -> Text
addTextMacron t = Text.append "_" t

addLaTeXMacron :: Text -> Text
addLaTeXMacron t = Text.concat ["\\_{", t, "}"]

-- | Create battery of tests for conversion to Unicode, to work with both
--   Unicode and LaTeX output formats.  We use the constructors here, rather
--   than parsing, to avoid an unfortunate circularity: we use the output
--   methods in the parser tests, so to use the parser here would be
--   ill-founded.
makeUnicodeTests :: (Text -> Text) -> (Letter -> Text) -> (Word -> Text)
                    -> [Test]
makeUnicodeTests addMacron convertLetter convertWord =
  ["base letter" ~:
    assertNormalizedEqual "α"
    (convertLetter (makeLetter 'α' NoBreathing NoAccent NoIotaSub NoMacron)),
    "acute" ~:
    assertNormalizedEqual "ί"
    (convertLetter (makeLetter 'ι' NoBreathing Acute NoIotaSub NoMacron)),
    "grave" ~:
    assertNormalizedEqual "ὶ"
    (convertLetter (makeLetter 'ι' NoBreathing Grave NoIotaSub NoMacron)),
    "circumflex" ~:
    assertNormalizedEqual "ῶ"
    (convertLetter (makeLetter 'ω' NoBreathing Circumflex NoIotaSub
                      NoMacron)),
    "smooth" ~:
    assertNormalizedEqual "ὀ"
    (convertLetter (makeLetter 'ο' Smooth NoAccent NoIotaSub NoMacron)),
    "rough" ~:
    assertNormalizedEqual "ῥ"
    (convertLetter (makeLetter 'ρ' Rough NoAccent NoIotaSub NoMacron)),
    "iota sub" ~:
    assertNormalizedEqual "ῃ"
    (convertLetter (makeLetter 'η' NoBreathing NoAccent IotaSub NoMacron)),
    "macron" ~:
    assertNormalizedEqual (addMacron "α")
    (convertLetter (makeLetter 'α' NoBreathing NoAccent NoIotaSub Macron)),
    "christmas tree" ~:
    assertNormalizedEqual "ᾄ"
    (convertLetter (makeLetter 'α' Smooth Acute IotaSub NoMacron)),
    "christmas tree with macron" ~:
    assertNormalizedEqual (addMacron "ἴ")
    (convertLetter (makeLetter 'ι' Smooth Acute NoIotaSub Macron)),

    "word" ~:
    assertNormalizedEqual (Text.concat ["ἀγορ", addMacron "ά", "ς"])
    (convertWord (makeWord
                  [makeLetter 'α' Smooth NoAccent NoIotaSub NoMacron,
                   makeLetter 'γ' NoBreathing NoAccent NoIotaSub NoMacron,
                   makeLetter 'ο' NoBreathing NoAccent NoIotaSub NoMacron,
                   makeLetter 'ρ' NoBreathing NoAccent NoIotaSub NoMacron,
                   makeLetter 'α' NoBreathing Acute NoIotaSub Macron,
                   makeLetter 'ς' NoBreathing NoAccent NoIotaSub NoMacron])),

    "christmas tree word" ~:
    assertNormalizedEqual "ῥᾆᾧᾔᾓό"
    (convertWord (makeWord
                  [makeLetter 'ρ' Rough NoAccent NoIotaSub NoMacron,
                   makeLetter 'α' Smooth Circumflex IotaSub NoMacron,
                   makeLetter 'ω' Rough Circumflex IotaSub NoMacron,
                   makeLetter 'η' Smooth Acute IotaSub NoMacron,
                   makeLetter 'η' Rough Grave IotaSub NoMacron,
                   makeLetter 'ο' NoBreathing Acute NoIotaSub NoMacron]))]

-- | Asserts that two Unicode strings are equal up to normalization.
assertNormalizedEqual :: Text    -- ^ Expected result
                         -> Text -- ^ Actual result
                         -> Test
assertNormalizedEqual expected actual =
  (normalize expected) ~=? (normalize actual)
