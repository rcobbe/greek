{-# LANGUAGE MultiWayIf #-}

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

-- | Normalization logic for polytonic Greek text in Unicode.  Using the ICU
--   bindings doesn't work, for two reasons:
--
--   (1) Linking nonsense with libiconv on MacOS/MacPorts.  See
--   <http://blog.omega-prime.co.uk/?p=96> for a partial discussion of the
--   issues.  Unfortunately, none of the three alternatives proposed there
--   works in my case: I need libiconv for other MacPorts packages, linking
--   against the system libiconv doesn't work, and the version of GHC in
--   MacPorts is ancient.
--
--   (2) The primary application in which I wanted to use Unicode requires that
--   the combining characters appear in a particular order, but Unicode's
--   notion of normalized order doesn't actually enforce this.  In particular,
--   attempting to normalize @\"\\x3b1\\x300\\x314\"@ into NFD form returns
--   @\"\\x3b1\\x300\\x314\"@, but normalizing @\"\\x3b1\\x314\\x300\"@ returns
--   @\"\\x3b1\\x314\\x300\"@, which means that I can't rely on the order of
--   the combining characters.  NFC normalization has similar problems:
--   converting @\"\\x3b1\\x300\\x314\"@ (lowercase alpha, combining grave,
--   combining rough breathing) to NFC gives @\"\\x1f70\\x314\"@ (precomposed
--   alpha-grave, combining rough-breathing), and converting
--   @\"\\x3b1\\x314\\x300\"@ gives @\"\\x1f03\"@ (precomposed
--   alpha-rough-grave).
module Data.Greek.Normalize(normalize) where

import Data.Set (Set, member)
import qualified Data.Set as Set

import Data.Textual (Textual)
import qualified Data.Textual as Textual

import Data.Greek.UnicodeData

-- | Normalizes all polytonic Greek text in a string.  Each maximal substring
--   of the input that consists of a single Greek base character followed by
--   zero or more Greek combining diacriticals is replaced with the equivalent
--   sequence that contains a single bare Greek letter followed by zero or more
--   Greek combining diacriticals.  In the output, a contiguous sequence of
--   Greek diacriticals never contains any duplicates, and the diacriticals in
--   a sequence always appear in an order consistent with the following:
--   \\x0313, \\x0314, \\x0308, \\x0301, \\x0300, \\x0342, \\x0345 -- that is,
--   smooth, rough, dieresis, acute, grave, circumflex, iota subscript.
--
--   [Greek base character:] a bare Greek letter, or one of \\x1f00-\\x1faf,
--   \\x1fb2-\\x1fb7, \\x1fba-\\x1fbc, \\x1fc2-\\x1fcc, \\x1fd2-\\x1fd7,
--   \\x1fda-\\x1fdb, \\x1fe2-\\x1fe7, \\x1fea-\\x1fec, or \\x1ff2-\\x1ffc
--   (minus any of the reserved Unicode code points in those ranges).  Contains
--   all Greek letters, with or without precomposed diacriticals, except
--   for the macron or breve (vrachy) marks.
--
--   [Greek combining diacritical:] one of \\x0313 (smooth), \\x0314 (rough),
--   \\x0308 (dialytika), \\x0301 (acute), \\x0300 (grave), \\x0342
--   (circumflex), \\x0345 (iota subscript).
--
--   [Bare Greek letter:] one of \\x0391-\\x03a9, \\x03b1-\\x03c9, \\x03dc, or
--   \\x03dd, minus any of the reserved Unicode code points in those ranges.
--   Contains all Greek letters, including digamma, without any precomposed
--   diacriticals.)
--
--   This function makes no attempt to detect or report invalid combinations of
--   diacriticals, including, e.g., multiple accent marks (whether distinct or
--   otherwise).  Any characters that do not match the pattern described above
--   are copied unchanged to the output.  This function only re-orders and
--   de-dups Greek combining diacriticals when they appear after a Greek base
--   character; otherwise, it copies them to the output unchanged.
normalize :: Textual a => a -> a
normalize t =
  if Textual.null t then Textual.empty
  else
    let char = Textual.head t
        chars = Textual.tail t
        (rawCombChars, rest) = getCombiningChars chars
        dialytikaCombChars = Set.insert combDialytika rawCombChars
    in
      if
        | char == '\x03aa' ->  -- cap iota with dialytika
          Textual.append
            (normalizeChar capIota dialytikaCombChars)
            (normalize rest)
        | char == '\x03ab' ->  -- cap upsilon with dialytica
          Textual.append
            (normalizeChar capUpsilon dialytikaCombChars)
            (normalize rest)
        | char == '\x03ca' ->  -- lowercase iota with dialytika
          Textual.append
            (normalizeChar baseIota dialytikaCombChars)
            (normalize rest)
        | char == '\x03cb' ->  -- lowercase upsilon with dialytika
          Textual.append
            (normalizeChar baseUpsilon dialytikaCombChars)
            (normalize rest)
        | char `member` baseChars ->   -- letter with no diacriticals
          Textual.append (normalizeChar char rawCombChars) (normalize rest)
        | char `member` greekExtendedChars ->
          -- letter w/ precomp. diacriticals
          Textual.append
            (normalizeChar
              (base char)
              (Set.union (precomposedDiacriticals char) rawCombChars))
            (normalize rest)
        | otherwise -> Textual.cons char (normalize chars)

-- | Returns a set containing the characters from the longest prefix of the
--   input that contains only Greek diacritical characters, and the
--   rest of the input.
getCombiningChars :: Textual a => a -> (Set Char, a)
getCombiningChars chars =
  let (combiningChars, rest) = Textual.span isCombiningChar chars
  in (Set.fromList (Textual.toString combiningChars), rest)

-- | Recognizes combining Greek diacriticals.
isCombiningChar :: Char -> Bool
isCombiningChar c =
  c `elem` [combAcute, combGrave, combCirc, combSmooth, combRough,
            combIotaSub, combDialytika]

-- | Renders a base character and diacriticals in normalized form.
normalizeChar :: Textual a => Char -> Set Char -> a
normalizeChar base combiningChars =
  Textual.cons
    base
    (Textual.fromString (filter charAppears orderedCombiningChars))
  where charAppears c = c `member` combiningChars

-- | Computes the set of diacriticals implicit in a precomposed character.
--   Produces correct results only for members of 'greekExtendedChars'.
precomposedDiacriticals :: Char -> Set Char
precomposedDiacriticals c =
  Set.fromList (
    concat [if hasAcute c     then [combAcute]     else [],
            if hasGrave c     then [combGrave]     else [],
            if hasCirc c      then [combCirc]      else [],
            if hasDialytika c then [combDialytika] else [],
            if hasSmooth c    then [combSmooth]    else [],
            if hasRough c     then [combRough]     else [],
            if hasIotaSub c   then [combIotaSub]   else []])

-- | List of Unicode combining diacriticals used for Greek, in the normalized
--   order.
orderedCombiningChars :: [Char]
orderedCombiningChars =
  [combSmooth, combRough,
   combDialytika,
   combAcute, combGrave, combCirc,
   combIotaSub]

-- | Set of Greek letters with no diacriticals.
baseChars :: Set Char
baseChars = Set.unions (map Set.fromList [[capAlpha .. capRho],
                                          [capSigma .. capOmega],
                                          [baseAlpha .. baseOmega],
                                          [capDigamma, baseDigamma]])

-- | Recognizes characters in 'greekExtendedChars' that have acute accents.
hasAcute :: Char -> Bool
hasAcute c =
  let (sixteens, ones) = digits c
  in if | sixteens == 0x7            -> odd ones
        | ones == 0x3                -> sixteens == 0xD || sixteens == 0xE
        | ones == 0x4 || ones == 0x5 -> sixteens /= 0xE
        | ones == 0x9                -> sixteens == 0xC || sixteens == 0xF
        | ones == 0xB                -> sixteens >= 0xB
        | ones == 0xC || ones == 0xD -> sixteens <= 0xA
        | otherwise                  -> False

-- | Recognizes chars in 'greekExtendedChars' that have grave accents.
hasGrave :: Char -> Bool
hasGrave c =
  let (sixteens, ones) = digits c
  in if | sixteens == 0x7 -> even ones
        | ones == 0x2     -> True
        | ones == 0x3     -> sixteens <= 0xA
        | ones == 0x8     -> sixteens == 0xC || sixteens == 0xF
        | ones == 0xA     -> True
        | ones == 0xB     -> sixteens <= 0xA
        | otherwise       -> False

-- | Recognizes chars in 'greekExtendedChars' that have circumflex accents.
hasCirc :: Char -> Bool
hasCirc c =
  let (sixteens, ones) = digits c
  in sixteens /= 0x7
     && (ones == 0x6 || ones == 0x7 || ones == 0xE || ones == 0xF)

-- | Recognizes chars in 'greekExtendedChars' that have dialytikas.
hasDialytika :: Char -> Bool
hasDialytika c = c `elem` ("\x1fd2\x1fd3\x1fd7\x1fe2\x1fe3\x1fe7" :: String)

-- | Computes the base character, with no precomposed combining diacriticals,
--   for characters in 'greekExtendedChars'.
base :: Char -> Char
base c =
  let (sixteens, ones) = digits c
  in case sixteens of
    0x0 | ones <= 0x7 -> baseAlpha
        | otherwise   -> capAlpha
    0x1 | ones <= 0x7 -> baseEpsilon
        | otherwise   -> capEpsilon
    0x2 | ones <= 0x7 -> baseEta
        | otherwise   -> capEta
    0x3 | ones <= 0x7 -> baseIota
        | otherwise   -> capIota
    0x4 | ones <= 0x7 -> baseOmicron
        | otherwise   -> capOmicron
    0x5 | ones <= 0x7 -> baseUpsilon
        | otherwise   -> capUpsilon
    0x6 | ones <= 0x7 -> baseOmega
        | otherwise   -> capOmega
    0x7 | ones <= 0x1 -> baseAlpha
        | ones <= 0x3 -> baseEpsilon
        | ones <= 0x5 -> baseEta
        | ones <= 0x7 -> baseIota
        | ones <= 0x9 -> baseOmicron
        | ones <= 0xB -> baseUpsilon
        | otherwise   -> baseOmega
    0x8 | ones <= 0x7 -> baseAlpha
        | otherwise   -> capAlpha
    0x9 | ones <= 0x7 -> baseEta
        | otherwise   -> capEta
    0xA | ones <= 0x7 -> baseOmega
        | otherwise   -> capOmega
    0xB | ones <= 0x7 -> baseAlpha
        | otherwise   -> capAlpha
    0xC | ones <= 0x7 -> baseEta
        | ones <= 0x9 -> capEpsilon
        | otherwise   -> capEta
    0xD | ones <= 0x7 -> baseIota
        | otherwise   -> capIota
    0xE | ones <= 0x3 -> baseUpsilon
        | ones <= 0x5 -> baseRho
        | ones <= 0x7 -> baseUpsilon
        | ones <= 0xB -> capUpsilon
        | otherwise   -> capRho
    0xF | ones <= 0x7 -> baseOmega
        | ones <= 0x9 -> capOmicron
        | otherwise   -> capOmega
    _ -> error ("base: 'sixteens' out of range (" ++ show sixteens ++ ")")

-- | Recognizes chars in 'greekExtendedChars' that have smooth breathing.
hasSmooth :: Char -> Bool
hasSmooth c =
  let (sixteens, ones) = digits c
  in (sixteens /= 0x7 && sixteens <= 0xA && even ones)
     || c == '\x1FE4'

-- | Recognizes chars in 'greekExtendedChars' that have rough breathing.
hasRough :: Char -> Bool
hasRough c =
  let (sixteens, ones) = digits c
  in (sixteens /= 0x7 && sixteens <= 0xA && odd ones)
     || c == '\x1FE5'
     || c == '\x1FEC'

-- | Recognizes chars in 'greekExtendedChars' that have iota subscript.
hasIotaSub :: Char -> Bool
hasIotaSub c =
  let (sixteens, ones) = digits c
      inWideRow = ones `elem` [0x2, 0x3, 0x4, 0x7, 0xC]
      inNormalColumn = 0x8 <= sixteens && sixteens <= 0xA
      inWideColumn = sixteens == 0xB || sixteens == 0xC || sixteens == 0xF
  in inNormalColumn || (inWideColumn && inWideRow)

-- | Computes the sixteens & ones digits of the given character's Unicode code
--   point.
digits :: Char -> (Int, Int)
digits c =
  let n = fromEnum c in ((n `div` 16) `mod` 16, n `mod` 16)
