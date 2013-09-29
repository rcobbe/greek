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

import Control.Exceptional

import Data.Textual (Textual)
import qualified Data.Textual as Textual
import Data.Greek.Word
import Data.Greek.Normalize
import Data.Greek.UnicodeData

-- I used to use Parsec, but the auto-generated error messages aren't actually
-- all that helpful.  In particular, every time a parser failed, it stuck the
-- error message on the front of the list, so errors tended to look like
-- ["greek letter", "cannot use macron and circumflex together"].
--
-- I also tried Text.Regex.Posix to parse out each individual letter, but that
-- library can't handle Unicode characters in the regular expressions.  So, do
-- it by hand.

-- | Signals an error in converting text to greek representation.  The offsets
--   below are in terms of _Greek_ letters, not in terms of the characters in
--   the input, as the automatic normalization affects the latter.
data ParseError = EmptyInput
                  -- ^ Input string is empty
                | MultipleBreathing { offset :: Int, diacriticals :: [Char] }
                  -- ^ The character at the indicated location has multiple
                  --   breathing marks
                | MultipleAccent { offset :: Int, diacriticals :: [Char] }
                  -- ^ The character at the indicated offset has multiple
                  --   accents.
                | InvalidMacron { offset :: Int, base :: Char }
                  -- ^ Explicit macron is not valid on the indicated character
                | InvalidBreathing { offset :: Int, base :: Char }
                  -- ^ Breathing mark is not valid on the indicated character
                | InvalidAccent { offset :: Int, base :: Char }
                  -- ^ Accent is not valid on the indicated character.  This
                  --   includes both consonants with any accent at all, and
                  --   also circumflex on epsilon or omicron.
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
                  -- ^ There's remaining input after parsing a single letter.
                | UnsupportedChar { offset :: Int, char :: Char }
                  -- ^ Input at indicated offset is not a Greek letter;
                  --   suppplied character is the erroneous input.
                | MissingLetter { offset :: Int }
                  -- ^ The letter at the indicated offset is missing its base
                  --   letter.
                | InternalError { offset :: Int, msg :: String }
                  -- ^ General internal error; shouldn't happen.
                deriving (Eq, Show)

-- | Parses a string to a Greek word.  All characters in input should be valid
--   Greek, although the input doesn't have to be normalized.
word :: Textual a => a -> Exceptional ParseError Word
word src =
  do letters <- wordLoop 0 (Textual.toString (normalize src))
     when (null letters) (throw EmptyInput)
     return $ makeWord letters

-- | Parses a string to a single Greek letter; the input should be exactly the
--   Greek for that letter and nothing else.  The input does not need to be
--   normalized.
letter :: Textual a => a -> Exceptional ParseError Letter
letter src =
  do when (Textual.null src) (throw EmptyInput)
     (letter, rest) <-
       addOffset 0 (parseLetter (Textual.toString (normalize src)))
     unless (null rest) (throw $ TrailingInput 1 rest)
     return letter

-- | Variant of 'word' that aborts on parse errors, so use this only for
--   literal Greek words in code and not for user input.
literalWord :: Textual a => a -> Word
literalWord src = run' (word src)

-- | Variant of 'letter' that aborts on parse errors, so use this only for
--   literal Greek letters in code and not for user input.
literalLetter :: Textual a => a -> Letter
literalLetter src = run' (letter src)

-- | Main loop for parsing a Greek word.  The first argument is the
--   _letter_-based index of the string, for error messages.
wordLoop :: Int -> String -> Exceptional ParseError [Letter]
wordLoop _ [] = return []
wordLoop index str =
  do (letter, rest) <- addOffset index (parseLetter str)
     restOutput <- wordLoop (index + 1) rest
     return $ letter : restOutput

-- | Adds the specified offset to an exceptional result of the supplied
--   computation, if any.
addOffset :: Int -> Exceptional ParseError a -> Exceptional ParseError a
addOffset index exceptionalComp =
  transformException (addOffset' index) exceptionalComp
  where addOffset' index exn = exn { offset = index }

-- | Parses a single letter from a string.  On success, returns parsed letter
--   and unparsed input.
parseLetter :: String -> Exceptional ParseError (Letter, String)
parseLetter str =
  do (rawLetter, rest) <- extractRawLetter str
     firstLetter <-
       case Map.lookup (rl_base rawLetter) parserTable of
         Just parser -> parser rawLetter
         Nothing -> parseConsonant rawLetter
     return (firstLetter, rest)

----------------------------------------------------------------------
--
-- extracting the first letter and its component parts
--
----------------------------------------------------------------------

-- Raw letter from input.  Not guaranteed to be a valid Greek letter.
data RawLetter = RawLetter { rl_macron :: Macron,
                             rl_base :: Char,
                             rl_breathing :: Breathing,
                             rl_accent :: Accent,
                             rl_iotaSub :: IotaSub }

extractRawLetter :: String -> Exceptional ParseError (RawLetter, String)
extractRawLetter [] =
  throw $ InternalError { offset = -1, msg = "extractRawLetter: empty input" }
extractRawLetter str =
  do (macron, str) <- extractMacron str
     (baseChar, str) <- extractBaseChar str
     (breathing, str) <- extractBreathing str
     (accent, str) <- extractAccent str
     (iotaSub, str) <- extractIotaSub str
     return (RawLetter macron baseChar breathing accent iotaSub, str)

extractMacron :: String -> Exceptional ParseError (Macron, String)
extractMacron s =
  case span (== '_') s of
    ("", rest) -> return (NoMacron, rest)
    ("_", rest) -> return (Macron, rest)
    (bogus, _) ->
      -- normalization should dedup all diacriticals
      throw $ (InternalError { offset = -1,
                               msg = "multiple macrons" })

extractBaseChar :: String -> Exceptional ParseError (Char, String)
extractBaseChar [] = throw $ MissingLetter { offset = -1 }
extractBaseChar (c : rest)
  | c `Set.member` baseChars = return (c, rest)
  | otherwise = throw $ UnsupportedChar { offset = -1, char = c }

extractBreathing :: String -> Exceptional ParseError (Breathing, String)
extractBreathing s =
  case span isBreathing s of
    ("", rest) -> return (NoBreathing, rest)
    ([c], rest)
      | c == combSmooth -> return (Smooth, rest)
      | c == combRough -> return (Rough, rest)
      | otherwise ->
          throw
            (InternalError { offset = -1,
                             msg = "extractBreathing: invalid char: " ++ [c] })
    (bogus, _) -> throw $ MultipleBreathing { offset = -1,
                                              diacriticals = bogus }
  where isBreathing :: Char -> Bool
        isBreathing c =
          c == combSmooth || c == combRough

extractAccent :: String -> Exceptional ParseError (Accent, String)
extractAccent s =
  case span isAccent s of
    ("", rest) -> return (NoAccent, rest)
    ([c], rest)
      | c == combAcute -> return (Acute, rest)
      | c == combGrave -> return (Grave, rest)
      | c == combCirc -> return (Circumflex, rest)
      | otherwise ->
          throw
            (InternalError { offset = -1,
                             msg = "extractAccent: invalid char: " ++ [c] })
    (bogus, _) -> throw $ MultipleAccent { offset = -1,
                                           diacriticals = bogus }
  where isAccent :: Char -> Bool
        isAccent c =
          c == combAcute || c == combGrave || c == combCirc

extractIotaSub :: String -> Exceptional ParseError (IotaSub, String)
extractIotaSub s =
  case span (== combIotaSub) s of
    ("", rest) -> return (NoIotaSub, rest)
    ([_], rest) -> return (IotaSub, rest)
    (bogus, _) ->
      throw $ (InternalError { offset = -1,
                               msg = "multiple iota subscripts" })

----------------------------------------------------------------------
--
-- Converting a raw letter to a Greek letter
--
----------------------------------------------------------------------

type ParseFunction = RawLetter -> Exceptional ParseError Letter

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
  (capOmicron, parseEpsilonOmicron),
  (baseUpsilon, parseIotaUpsilon),
  (capUpsilon, parseIotaUpsilon),
  (baseOmega, parseEtaOmega),
  (capOmega, parseEtaOmega),
  (baseRho, parseRho),
  (capRho, parseRho)]

parseAlpha :: ParseFunction
parseAlpha (RawLetter macron base breathing accent iotaSub) =
  do when (macron == Macron && accent == Circumflex)
       (throw $ MacronWithCircumflex { offset = -1 })
     when (macron == Macron && iotaSub == IotaSub)
       (throw $ MacronWithIotaSub { offset = -1 })
     return $ makeLetter base breathing accent iotaSub macron

parseEpsilonOmicron :: ParseFunction
parseEpsilonOmicron (RawLetter macron base breathing accent iotaSub) =
  do when (macron == Macron)
       (throw $ InvalidMacron (-1) base)
     when (accent == Circumflex)
       (throw $ InvalidAccent (-1) base)
     when (iotaSub == IotaSub)
       (throw $ InvalidIotaSub (-1) base)
     return $ makeLetter base breathing accent iotaSub macron

parseEtaOmega :: ParseFunction
parseEtaOmega (RawLetter macron base breathing accent iotaSub) =
  do when (macron == Macron)
       (throw $ InvalidMacron (-1) base)
     return $ makeLetter base breathing accent iotaSub macron

parseIotaUpsilon :: ParseFunction
parseIotaUpsilon (RawLetter macron base breathing accent iotaSub) =
  do when (iotaSub == IotaSub)
       (throw $ InvalidIotaSub (-1) base)
     when (macron == Macron && accent == Circumflex)
       (throw $ MacronWithCircumflex (-1))
     return $ makeLetter base breathing accent iotaSub macron

parseRho :: ParseFunction
parseRho (RawLetter macron base breathing accent iotaSub) =
  do when (macron == Macron)
       (throw $ InvalidMacron (-1) base)
     when (accent /= NoAccent)
       (throw $ InvalidAccent (-1) base)
     when (iotaSub == IotaSub)
       (throw $ InvalidIotaSub (-1) base)
     return $ makeLetter base breathing accent iotaSub macron

-- | Parses a single non-rho consonant, ensuring no diacriticals.
parseConsonant :: ParseFunction
parseConsonant (RawLetter macron base breathing accent iotaSub) =
  do when (macron == Macron)
       (throw $ InvalidMacron (-1) base)
     when (breathing /= NoBreathing)
       (throw $ InvalidBreathing (-1) base)
     when (accent /= NoAccent)
       (throw $ InvalidAccent (-1) base)
     when (iotaSub == IotaSub)
       (throw $ InvalidIotaSub (-1) base)
     return $ makeLetter base breathing accent iotaSub macron
