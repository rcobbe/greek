{-# LANGUAGE OverloadedStrings #-}

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
module Data.Greek.Private.Normalize where

import Data.Set (Set, member)
import qualified Data.Set as Set

import Data.Greek.Texty (Texty)
import qualified Data.Greek.Texty as Texty
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
--   [Greek base character:] one of \\x0391-\\x03a9, \\x03b1-\\x03c9, \\x03dc,
--   \\x03de, \\x1f00-\\x1faf, \\x1fb2-\\x1fb7, \\x1fba-\\x1fbc,
--   \\x1fc2-\\x1fcc, \\x1fd2-\\x1fd7, \\x1fda-\\x1fdb, \\x1fe2-\\x1fe7,
--   \\x1fea-\\x1fec, or \\x1ff2-\\x1ffc (minus any of the reserved Unicode
--   code points in those ranges).
--
--   [Greek combining diacritical:] one of \\x0313 (smooth), \\x0314 (rough),
--   \\x0308 (dialytika), \\x0301 (acute), \\x0300 (grave), \\x0342
--   (circumflex), \\x0345 (iota subscript).
--
--   [Bare Greek letter:] one of \\x0391-\\x03a9, \\x03b1-\\x03c9, \\x03dc, or
--   \\x03de.
--
--   This function makes no attempt to detect or report invalid combinations of
--   diacriticals, including, e.g., multiple accent marks (whether distinct or
--   otherwise).  Any characters that do not match the pattern described above
--   are copied unchanged to the output.  This function only re-orders and
--   de-dups Greek combining diacriticals when they appear after a Greek base
--   character; otherwise, it copies them to the output unchanged.
normalize :: Texty a => a -> a
normalize t =
  if Texty.null t then Texty.empty
  else
    let char = Texty.head t
        chars = Texty.tail t
        (rawCombChars, rest) = getCombiningChars chars
        dialytikaCombChars = Set.insert combDialytika rawCombChars
  in case char of
    '\x03aa' ->
      Texty.append (normalizeChar capIota dialytikaCombChars) (normalize rest)
    '\x03ab' ->
      Texty.append (normalizeChar capUpsilon dialytikaCombChars)
                   (normalize rest)
    '\x03ca' ->
      Texty.append (normalizeChar baseIota dialytikaCombChars) (normalize rest)
    '\x03cb' ->
      Texty.append (normalizeChar baseUpsilon dialytikaCombChars)
                   (normalize rest)
    _ | char `member` baseChars ->
        Texty.append (normalizeChar char rawCombChars) (normalize rest)
      | char `member` greekExtendedChars ->
        Texty.append (
          normalizeChar (base char) (Set.union (precomposedDiacriticals char)
                                     rawCombChars))
          (normalize rest)
      | otherwise -> Texty.cons char (normalize chars)

-- | Returns the maximal prefix of the input that contains only Greek
--   diacritical characters, as a set, and the rest of the input as a list.
getCombiningChars :: Texty a => a -> (Set Char, a)
getCombiningChars chars =
  let (combiningChars, rest) = Texty.span isCombiningChar chars
  in (Set.fromList (Texty.unpack combiningChars), rest)

-- | Recognizes combining Greek diacriticals.
isCombiningChar :: Char -> Bool
isCombiningChar c =
  c `elem` [combAcute, combGrave, combCirc, combSmooth, combRough,
            combIotaSub, combDialytika]

-- | Renders a base character and diacriticals in normalized form.
normalizeChar :: Texty a => Char -> Set Char -> a
normalizeChar base combiningChars =
  Texty.cons base (Texty.pack (filter charAppears orderedCombiningChars))
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

-- | Chars in the Greek Extended Unicode range that we treat specially during
--   normalization.
greekExtendedChars :: Set Char
greekExtendedChars =
  Set.unions (
    map Set.fromList [
       ['\x1f00'..'\x1f15'],
       ['\x1f18'..'\x1f1d'],
       ['\x1f20'..'\x1f45'],
       ['\x1f48'..'\x1f4d'],
       ['\x1f50'..'\x1f57'],
       ['\x1f59', '\x1f5b', '\x1f5d', '\x1f5f'],
       ['\x1f60'..'\x1f7d'],
       ['\x1f80'..'\x1faf'],
       ['\x1fb2'..'\x1fb4'],
       ['\x1fb6', '\x1fb7', '\x1fba', '\x1fbb', '\x1fbc'],
       ['\x1fc2', '\x1fc3', '\x1fc4'],
       ['\x1fc6'..'\x1fcc'],
       ['\x1fd2', '\x1fd3', '\x1fd6', '\x1fd7', '\x1fda', '\x1fdb'],
       ['\x1fe2'..'\x1fe7'],
       ['\x1fea'..'\x1fec'],
       ['\x1ff2'..'\x1ff4'],
       ['\x1ff6'..'\x1ffc']])

-- | Recognizes characters in 'greekExtendedChars' that have acute accents.
hasAcute :: Char -> Bool
hasAcute c =
  let (sixteens, ones) = digits c
  in case () of
    _ | sixteens == 0x7            -> odd ones
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
  in case () of
    _ | sixteens == 0x7 -> even ones
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
hasDialytika c = c `elem` "\x1fd2\x1fd3\x1fd7\x1fe2\x1fe3\x1fe7"

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
