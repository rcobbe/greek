-- | Parses external representations of Greek Text.  The input should be
--   Unicode text, containing only Greek letters and combining diacriticals, in
--   the format defined by 'Data.Greek.Normalize.normalize'.  We also allow
--   underscores immediately before certain vowels, to indicate that the vowel
--   is long.  (The underscore corresponds to the presence of a macron in the
--   written representation, and not to vowel length -- so, for instance,
--   \"_ά\" denotes a long alpha with an acute accent, but \"_ᾶ\" results in a
--   parse error, because the macron is redundant with the circumflex.)
module Data.Greek.Parser(ParseError(..),
                         word,
                         letter,
                         literalWord,
                         literalLetter) where

import Control.Monad
import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Control.Exceptional (Exceptional)
import qualified Control.Exceptional as Exc

import Text.Regex.Posix

import Data.Greek.Texty (Texty)
import qualified Data.Greek.Texty as Texty
import Data.Greek.Word
import Data.Greek.Normalize
import Data.Greek.UnicodeData

-- I used to use Parsec, but the auto-generated error messages aren't actually
-- all that helpful.  In particular, every time a parser failed, it stuck the
-- error message on the front of the list, so errors tended to look like
-- ["greek letter", "cannot use macron and circumflex together"].

-- | Signals an error in converting text to greek representation.  The offsets
--   below are in terms of _Greek_ letters, not in terms of the characters in
--   the input, as the automatic normalization affects the latter.
data ParseError = EmptyInput
                  -- ^ Input string is empty
                | InvalidMacron { offset :: Int, base :: Char }
                  -- ^ Explicit macron is not valid on the indicated character
                | InvalidBreathing { offset :: Int, base :: Char }
                  -- ^ Breathing mark is not valid on the indicated character
                | MultipleBreathing { offset :: Int, diacriticals :: [Char] }
                  -- ^ The character at the indicated location has multiple
                  --   breathing marks
                | InvalidAccent { offset :: Int, base :: Char }
                  -- ^ Accent is not valid on the indicated character.  This
                  --   includes both consonants with any accent at all, and
                  --   also circumflex on epsilon or omicron.
                | MultipleAccent { offset :: Int, diacriticals :: [Char] }
                  -- ^ The character at the indicated offset has multiple
                  --   accents.
                | InvalidIotaSub { offset :: Int, base :: Char }
                  -- ^ Iota subscript is not valid on the indicated character.
                | MacronWithCircumflex { offset :: Int }
                  -- ^ The character at the indicated offset has both a macron
                  --   and a circumflex
                | MacronWithIotaSub { offset :: Int }
                  -- ^ The character at the indicated offset has both a macron
                  --   and an iota subscript.
                | TrailingInput { offset :: Int,
                                  text :: String }
                  -- ^ Input contains trailing characters that don't denote
                  --   Greek, beginning at the indicated offset
                | InvalidInput { offset :: Int,
                                 text :: String }
                  -- ^ Input at indicated offset is completely invald
                deriving (Eq, Show)

-- | Parses a string to a Greek word.  All characters in input should be valid
--   Greek, although the input doesn't have to be normalized.
word :: Texty a => a -> Exceptional ParseError Word
word src =
  do letters <- wordLoop 0 (Texty.toString (normalize src))
     when (null letters) (Exc.throw EmptyInput)
     return $ makeWord letters

-- | Parses a string to a single Greek letter; the input should be exactly the
--   Greek for that letter and nothing else.  The input does not need to be
--   normalized.
letter :: Texty a => a -> Exceptional ParseError Letter
letter src =
  do when (Texty.null src) (Exc.throw EmptyInput)
     (letter, rest) <- parseLetter 0 (Texty.toString (normalize src))
     unless (null rest) (Exc.throw $ TrailingInput 1 rest)
     return letter

-- | Variant of 'word' that aborts on parse errors, so use this only for
--   literal Greek words in code and not for user input.
literalWord :: Texty a => a -> Word
literalWord src = Exc.run' (word src)

-- | Variant of 'letter' that aborts on parse errors, so use this only for
--   literal Greek letters in code and not for user input.
literalLetter :: Texty a => a -> Letter
literalLetter src = Exc.run' (letter src)

-- | Main loop for parsing a Greek word.  The first argument is the
--   _letter_-based index of the string, for error messages.
wordLoop :: Int -> String -> Exceptional ParseError [Letter]
wordLoop _ [] = return []
wordLoop index str =
  do (letter, rest) <- parseLetter index str
     restOutput <- wordLoop (index + 1) rest
     return $ letter : restOutput

-- | Parses a single letter from a string, which may contain trailing input.
parseLetter :: Int -> String -> Exceptional ParseError (Letter, String)
parseLetter index str =
  case (str =~ letterRegex) :: (String, String, String, [String]) of
    (_, "", _, _) -> Exc.throw $ InvalidInput index str
    (_, _, rest, [macron, [base], breathing, accent, iotaSub]) ->
      do let submatches = Submatches macron base breathing accent iotaSub
         firstLetter <-
           case Map.lookup base parserTable of
             Just parser -> parser index submatches
             Nothing -> parseConsonant index submatches
         return (firstLetter, rest)
    (_, match, _, submatches) ->
      error
        (List.concat ["parseLetter (index ",
                      show index,
                      "): on matching ",
                      show match,
                      ", expected 5 submatches (w/ 2nd a single char); got ",
                      show submatches])

letterRegex :: String
letterRegex = List.concat ["^(_?)",
                           "([",
                           Set.elems baseChars,
                           "])",
                           "([\x0313\x0314]*)",
                           "([\x0301\x0300\x0342]*)",
                           "(\x0345?)"]

data Submatches = Submatches { sm_macron :: String,
                               sm_base :: Char,
                               sm_breathing :: String,
                               sm_accent :: String,
                               sm_iotaSub :: String }

type ParseFunction = Int -> Submatches -> Exceptional ParseError Letter

parserTable :: Map Char ParseFunction
parserTable = Map.fromList [
  (baseAlpha, parseAlpha),
  (capAlpha, parseAlpha),
  (baseEpsilon, parseEpsilonOmicron),
  (capEpsilon, parseEpsilonOmicron),
  (baseEta, parseEtaOmega),
  (capEta, parseEtaOmega),
  (baseIota, parseIotaUpsilon),
  (capIota, parseIotaUpsilon),
  (baseOmicron, parseEpsilonOmicron),
  (capOmega, parseEpsilonOmicron),
  (baseUpsilon, parseIotaUpsilon),
  (capUpsilon, parseIotaUpsilon),
  (baseOmega, parseEtaOmega),
  (capOmega, parseEtaOmega),
  (baseRho, parseRho),
  (capRho, parseRho)]

parseAlpha :: ParseFunction
parseAlpha index submatches =
  do let m = parseMacron index submatches
     b <- parseBreathing index submatches
     a <- parseAccent index submatches
     let i = parseIotaSub index submatches
     when (m == Macron && a == Circumflex)
       (Exc.throw $ MacronWithCircumflex index)
     when (m == Macron && i == IotaSub)
       (Exc.throw $ MacronWithIotaSub index)
     return $ makeLetter (sm_base submatches) b a i m

parseEpsilonOmicron :: ParseFunction
parseEpsilonOmicron index submatches =
  do assertNoDiacritical submatches sm_macron (InvalidMacron index)
     assertNoDiacritical submatches sm_iotaSub (InvalidIotaSub index)
     b <- parseBreathing index submatches
     a <- parseAccent index submatches
     let base = sm_base submatches
     when (a == Circumflex)
       (Exc.throw $ InvalidAccent index base)
     return $ makeLetter base b a NoIotaSub NoMacron

parseEtaOmega :: ParseFunction
parseEtaOmega index submatches =
  do assertNoDiacritical submatches sm_macron (InvalidMacron index)
     b <- parseBreathing index submatches
     a <- parseAccent index submatches
     let i = parseIotaSub index submatches
     return $ makeLetter (sm_base submatches) b a i NoMacron

parseIotaUpsilon :: ParseFunction
parseIotaUpsilon index submatches =
  do assertNoDiacritical submatches sm_iotaSub (InvalidIotaSub index)
     let m = parseMacron index submatches
     b <- parseBreathing index submatches
     a <- parseAccent index submatches
     when (a == Circumflex && m == Macron)
       (Exc.throw $ MacronWithCircumflex index)
     return $ makeLetter (sm_base submatches) b a NoIotaSub NoMacron

parseRho :: ParseFunction
parseRho index submatches =
  do assertNoDiacritical submatches sm_macron (InvalidMacron index)
     assertNoDiacritical submatches sm_accent (InvalidAccent index)
     assertNoDiacritical submatches sm_iotaSub (InvalidIotaSub index)
     b <- parseBreathing index submatches
     return $ makeLetter (sm_base submatches) b NoAccent NoIotaSub NoMacron

-- | Parses a single non-rho consonant, ensuring no diacriticals.
parseConsonant :: ParseFunction
parseConsonant index submatches =
  do assertNoDiacritical submatches sm_macron (InvalidMacron index)
     assertNoDiacritical submatches sm_breathing (InvalidBreathing index)
     assertNoDiacritical submatches sm_accent (InvalidAccent index)
     assertNoDiacritical submatches sm_iotaSub (InvalidIotaSub index)
     return $
       makeLetter (sm_base submatches) NoBreathing NoAccent NoIotaSub NoMacron

parseMacron :: Int -> Submatches -> Macron
parseMacron index (Submatches { sm_macron = m }) =
  case m of
    "" -> NoMacron
    "_" -> Macron
    bogus -> error (List.concat ["parseMacron (index ",
                                 show index,
                                 "): invalid macron: ",
                                 show bogus])

parseBreathing :: Int -> Submatches -> Exceptional ParseError Breathing
parseBreathing index submatches =
  case sm_breathing submatches of
    [] -> return NoBreathing
    [x]
      | x == combSmooth -> return Smooth
      | x == combRough -> return Rough
      | otherwise ->
        error (List.concat ["parseBreathing (index ",
                            show index,
                            "): invalid breathing mark: ",
                            show x])
    chars -> Exc.throw (MultipleBreathing index chars)

parseAccent :: Int -> Submatches -> Exceptional ParseError Accent
parseAccent index submatches =
  case sm_accent submatches of
    [] -> return NoAccent
    [x]
      | x == combAcute -> return Acute
      | x == combGrave -> return Grave
      | x == combCirc -> return Circumflex
      | otherwise ->
        error (List.concat ["parseAccent (index ",
                            show index,
                            "): invalid accent: ",
                            show x])
    chars -> Exc.throw (MultipleAccent index chars)

parseIotaSub :: Int -> Submatches -> IotaSub
parseIotaSub index submatches =
  case sm_iotaSub submatches of
    [] -> NoIotaSub
    (x : rest)
      | x == combIotaSub && rest == [] -> IotaSub
      | otherwise ->
        error (List.concat ["parseIotaSub (index ",
                            show index,
                            "): invalid iota sub: ",
                            show (x : rest)])

assertNoDiacritical :: Submatches -> (Submatches -> String)
                       -> (Char -> ParseError)
                       -> Exceptional ParseError ()
assertNoDiacritical submatches selector exn =
  unless (List.null (selector submatches))
    (Exc.throw (exn (sm_base submatches)))

