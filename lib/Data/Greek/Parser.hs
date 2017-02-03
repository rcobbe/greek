{-# LANGUAGE MultiWayIf, ViewPatterns #-}

-- | Parses external representations of Greek Text.  The input should be
--   Unicode text, containing only Greek letters and combining diacriticals, in
--   the format defined by 'Data.Greek.Normalize.normalize'.  We also allow
--   underscores immediately before certain vowels, to indicate that the vowel
--   is long.  (The underscore corresponds to the presence of a macron in the
--   written representation, and not to vowel length -- so, for instance,
--   \"_ά\" denotes a long alpha with an acute accent, but \"_ᾶ\" results in a
--   parse error, because the macron is redundant with the circumflex.)
module Data.Greek.Parser(ParseError(..),
                         errorToString,
                         word,
                         letter,
                         literalWord,
                         literalLetter,
                         validInputChars) where

import Prelude hiding (Word)
import Control.Monad

import Control.Monad.Trans.Except (Except)
import qualified Control.Monad.Trans.Except as Except

import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Data.List (intercalate)
import Data.Textual (Textual, View(..), view)
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

-- | Generates string representation of error suitable for user-visible output.
errorToString :: ParseError -> String
errorToString err = (offsetPrefix err) ++ (formatError err)

-- | Generates an "at offset N: " prefix for parser error messages, where
--   appropriate.
offsetPrefix :: ParseError -> String
offsetPrefix err
  | err == EmptyInput = ""
  | otherwise         = ("at offset " ++ (show (offset err)) ++ ": ")

-- | Formats the (non-offset) portion of the error into a message suitable for
--   users.
formatError :: ParseError -> String
formatError EmptyInput = "empty input string"
formatError (MultipleBreathing _ marks) =
  "multiple breathing marks: " ++ commaSepMap showBreathing marks
formatError (MultipleAccent _ accents) =
  "multiple accents: " ++ commaSepMap showAccent accents
formatError (InvalidMacron _ base) =
  "macron invalid on base letter " ++ [base]
formatError (InvalidBreathing _ base) =
  "breathing mark invalid on base letter " ++ [base]
formatError (InvalidAccent _ base) =
  "accent invalid on base letter " ++ [base]
formatError (InvalidIotaSub _ base) =
  "iota subscript invalid on base letter " ++ [base]
formatError (MacronWithCircumflex _) =
  "macron and circumflex may not both appear on the same letter"
formatError (MacronWithIotaSub _) =
  "macron and iota subscript may not both appear on the same letter"
formatError (TrailingInput _ extra) =
  "trailing input after an input letter: " ++ show extra
formatError (UnsupportedChar _ c) =
  "unsupported character in input: " ++ show c
formatError (MissingLetter _) =
  "missing base letter"
formatError (InternalError _ msg) =
  "internal greek parser error: " ++ msg

-- XXX what is keybinding in haskell mode to insert & align equals sign?

-- | Renders a breathing mark character into a more readable form for error
--   messages.
showBreathing :: Char -> String
showBreathing c
  | c == combSmooth = "smooth"
  | c == combRough  = "rough"
  | otherwise       = error ("showBreathing: invalid breathing: " ++ [c])

-- | Renders an accent combining character into a more readable form for error
--   messages.
showAccent :: Char -> String
showAccent c
  | c == combAcute = "acute"
  | c == combGrave = "grave"
  | c == combCirc  = "circumflex"
  | otherwise      = error ("showAccent: invalid accent: " ++ [c])

-- | Map a function over a list, then join the results into a comma-separated
--   sequence.
commaSepMap :: (a -> String) -> [a] -> String
commaSepMap f xs = intercalate ", " (map f xs)

-- | Parses a string to a Greek word.  All characters in input should be valid
--   Greek, although the input doesn't have to be normalized.
word :: Textual a => a -> Except ParseError Word
word src =
  do letters <- wordLoop 0 (normalize src)
     when (null letters) (Except.throwE EmptyInput)
     return $ makeWord letters

-- | Parses a string to a single Greek letter; the input should be exactly the
--   Greek for that letter and nothing else.  The input does not need to be
--   normalized.
letter :: Textual a => a -> Except ParseError Letter
letter src =
  do when (Textual.null src) (Except.throwE EmptyInput)
     (letter, rest) <-
       addOffset 0 (parseLetter (normalize src))
     unless (Textual.null rest)
       (Except.throwE $ TrailingInput 1 (Textual.toString rest))
     return letter

-- | Variant of 'word' that aborts on parse errors, so use this only for
--   literal Greek words in code and not for user input.
literalWord :: Textual a => a -> Word
literalWord src = runExcept (word src)

-- | Variant of 'letter' that aborts on parse errors, so use this only for
--   literal Greek letters in code and not for user input.
literalLetter :: Textual a => a -> Letter
literalLetter src = runExcept (letter src)

-- | Main loop for parsing a Greek word.  The first argument is the
--   _letter_-based index of the string, for error messages.
wordLoop :: Textual a => Int -> a -> Except ParseError [Letter]
wordLoop index txt =
  if | Textual.null txt -> return []
     | otherwise ->
        do (letter, rest) <- addOffset index (parseLetter txt)
           restOutput <- wordLoop (index + 1) rest
           return $ letter : restOutput

-- | Adds the specified offset to an exceptional result of the supplied
--   computation, if any.
addOffset :: Int -> Except ParseError a -> Except ParseError a
addOffset index exceptionalComp =
  Except.withExcept (addOffset' index) exceptionalComp
  where addOffset' index exn = exn { offset = index }

-- | Parses a single letter from a string.  On success, returns parsed letter
--   and unparsed input.
parseLetter :: Textual a => a -> Except ParseError (Letter, a)
parseLetter txt =
  do (rawLetter, rest) <- extractRawLetter txt
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

-- | Raw letter from input.  Not guaranteed to be a valid Greek letter.
data RawLetter = RawLetter { rl_macron :: Macron,
                             rl_base :: Char,
                             rl_breathing :: Breathing,
                             rl_accent :: Accent,
                             rl_iotaSub :: IotaSub }

-- | Extracts the first letter from the input without checking for validity.
--   Returns the extracted letter and the remaining input.
extractRawLetter :: Textual a => a -> Except ParseError (RawLetter, a)
extractRawLetter (view -> Empty) =
  Except.throwE $
    InternalError { offset = -1, msg = "extractRawLetter: empty input" }
extractRawLetter txt =
  do (macron, txt) <- extractMacron txt
     (baseChar, txt) <- extractBaseChar txt
     (breathing, txt) <- extractBreathing txt
     (accent, txt) <- extractAccent txt
     (iotaSub, txt) <- extractIotaSub txt
     return (RawLetter macron baseChar breathing accent iotaSub, txt)

-- | Extracts an optional macron from the input; returns macron and remaining
--   input.
extractMacron :: Textual a => a -> Except ParseError (Macron, a)
extractMacron txt =
  case Textual.span (== '_') txt of
    (view -> Empty, rest) -> return (NoMacron, rest)
    (view -> '_' :|: (view -> Empty), rest) -> return (Macron, rest)
    _ ->
      -- normalization should dedup all diacriticals
      Except.throwE $ InternalError { offset = -1, msg = "multiple macrons" }

extractBaseChar :: Textual a => a -> Except ParseError (Char, a)
extractBaseChar (view -> Empty) = Except.throwE $ MissingLetter { offset = -1 }
extractBaseChar (view -> c :|: rest)
  | c `Set.member` baseChars = return (c, rest)
  | otherwise = Except.throwE $ UnsupportedChar { offset = -1, char = c }
extractBaseChar _ = error "view-pattern error"

extractBreathing :: Textual a => a -> Except ParseError (Breathing, a)
extractBreathing s =
  case Textual.span isBreathing s of
    (view -> Empty, rest) -> return (NoBreathing, rest)
    (view -> c :|: (view -> Empty), rest)
      | c == combSmooth -> return (Smooth, rest)
      | c == combRough -> return (Rough, rest)
      | otherwise ->
          Except.throwE $
            InternalError { offset = -1,
                            msg = "extractBreathing: invalid char: " ++ [c] }
    (bogus, _) ->
      Except.throwE $
        MultipleBreathing { offset = -1, diacriticals = Textual.toString bogus }
  where isBreathing :: Char -> Bool
        isBreathing c =
          c == combSmooth || c == combRough

extractAccent :: Textual a => a -> Except ParseError (Accent, a)
extractAccent txt =
  case Textual.span isAccent txt of
    (view -> Empty, rest) -> return (NoAccent, rest)
    (view -> c :|: (view -> Empty), rest)
      | c == combAcute -> return (Acute, rest)
      | c == combGrave -> return (Grave, rest)
      | c == combCirc -> return (Circumflex, rest)
      | otherwise ->
          Except.throwE $
            InternalError { offset = -1,
                            msg = "extractAccent: invalid char: " ++ [c] }
    (bogus, _) ->
      Except.throwE $
        MultipleAccent { offset = -1, diacriticals = Textual.toString bogus }
  where isAccent :: Char -> Bool
        isAccent c =
          c == combAcute || c == combGrave || c == combCirc

extractIotaSub :: Textual a => a -> Except ParseError (IotaSub, a)
extractIotaSub txt =
  case Textual.span (== combIotaSub) txt of
    (view -> Empty, rest) -> return (NoIotaSub, rest)
    (view -> _ :|: (view -> Empty), rest) -> return (IotaSub, rest)
    (_, _) ->
      Except.throwE $ (InternalError { offset = -1,
                                       msg = "multiple iota subscripts" })

----------------------------------------------------------------------
--
-- Converting a raw letter to a Greek letter
--
----------------------------------------------------------------------

type ParseFunction = RawLetter -> Except ParseError Letter

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
       (Except.throwE $ MacronWithCircumflex { offset = -1 })
     when (macron == Macron && iotaSub == IotaSub)
       (Except.throwE $ MacronWithIotaSub { offset = -1 })
     return $ makeLetter base breathing accent iotaSub macron

parseEpsilonOmicron :: ParseFunction
parseEpsilonOmicron (RawLetter macron base breathing accent iotaSub) =
  do when (macron == Macron)
       (Except.throwE $ InvalidMacron (-1) base)
     when (accent == Circumflex)
       (Except.throwE $ InvalidAccent (-1) base)
     when (iotaSub == IotaSub)
       (Except.throwE $ InvalidIotaSub (-1) base)
     return $ makeLetter base breathing accent iotaSub macron

parseEtaOmega :: ParseFunction
parseEtaOmega (RawLetter macron base breathing accent iotaSub) =
  do when (macron == Macron)
       (Except.throwE $ InvalidMacron (-1) base)
     return $ makeLetter base breathing accent iotaSub macron

parseIotaUpsilon :: ParseFunction
parseIotaUpsilon (RawLetter macron base breathing accent iotaSub) =
  do when (iotaSub == IotaSub)
       (Except.throwE $ InvalidIotaSub (-1) base)
     when (macron == Macron && accent == Circumflex)
       (Except.throwE $ MacronWithCircumflex (-1))
     return $ makeLetter base breathing accent iotaSub macron

parseRho :: ParseFunction
parseRho (RawLetter macron base breathing accent iotaSub) =
  do when (macron == Macron)
       (Except.throwE $ InvalidMacron (-1) base)
     when (accent /= NoAccent)
       (Except.throwE $ InvalidAccent (-1) base)
     when (iotaSub == IotaSub)
       (Except.throwE $ InvalidIotaSub (-1) base)
     return $ makeLetter base breathing accent iotaSub macron

-- | Parses a single non-rho consonant, ensuring no diacriticals.
parseConsonant :: ParseFunction
parseConsonant (RawLetter macron base breathing accent iotaSub) =
  do when (macron == Macron)
       (Except.throwE $ InvalidMacron (-1) base)
     when (breathing /= NoBreathing)
       (Except.throwE $ InvalidBreathing (-1) base)
     when (accent /= NoAccent)
       (Except.throwE $ InvalidAccent (-1) base)
     when (iotaSub == IotaSub)
       (Except.throwE $ InvalidIotaSub (-1) base)
     return $ makeLetter base breathing accent iotaSub macron

----------------------------------------------------------------------
--
-- Utilities
--
----------------------------------------------------------------------

-- | Runs an 'Except' computation and returns the final value.  If the
--   computation terminates with an uncaught exception, calls 'error'.
runExcept :: Show e => Except e a -> a
runExcept exceptComp =
  case (Except.runExcept exceptComp) of
    Left exn  -> error ("Uncaught exception: " ++ show exn)
    Right val -> val

-- | All characters that are valid Greek input.  This doesn't include all
--   characters defined above, but only those that the parser knows how to
--   handle.
validInputChars =
  Set.fromList [combAcute,
                combGrave,
                combCirc,
                combSmooth,
                combRough,
                combIotaSub,
                combDialytika,
                combMacron,
                combBreve,
                baseIotaSub,
                baseSmooth,
                baseCirc,
                baseSmoothGrave,
                baseSmoothAcute,
                baseSmoothCirc,
                baseRoughGrave,
                baseRoughAcute,
                baseRoughCirc,
                baseGrave,
                baseAcute,
                baseRough,
                capAlpha,
                capBeta,
                capGamma,
                capDelta,
                capEpsilon,
                capDigamma,
                capZeta,
                capEta,
                capTheta,
                capIota,
                capKappa,
                capLambda,
                capMu,
                capNu,
                capXi,
                capOmicron,
                capPi,
                capRho,
                capSigma,
                capTau,
                capUpsilon,
                capPhi,
                capChi,
                capPsi,
                capOmega,
                baseAlpha,
                baseBeta,
                baseGamma,
                baseDelta,
                baseEpsilon,
                baseDigamma,
                baseZeta,
                baseEta,
                baseTheta,
                baseIota,
                baseKappa,
                baseLambda,
                baseMu,
                baseNu,
                baseXi,
                baseOmicron,
                basePi,
                baseRho,
                baseFinalSigma,
                baseMedialSigma,
                baseTau,
                baseUpsilon,
                basePhi,
                baseChi,
                basePsi,
                baseOmega,
                baseQuestion,
                baseSemicolon,
                alphaSmooth,
                alphaRough,
                alphaSmoothGrave,
                alphaRoughGrave,
                alphaSmoothAcute,
                alphaRoughAcute,
                alphaSmoothCirc,
                alphaRoughCirc,
                capAlphaSmooth,
                capAlphaRough,
                capAlphaSmoothGrave,
                capAlphaRoughGrave,
                capAlphaSmoothAcute,
                capAlphaRoughAcute,
                capAlphaSmoothCirc,
                capAlphaRoughCirc,
                epsilonSmooth,
                epsilonRough,
                epsilonSmoothGrave,
                epsilonRoughGrave,
                epsilonSmoothAcute,
                epsilonRoughAcute,
                capEpsilonSmooth,
                capEpsilonRough,
                capEpsilonSmoothGrave,
                capEpsilonRoughGrave,
                capEpsilonSmoothAcute,
                capEpsilonRoughAcute,
                etaSmooth,
                etaRough,
                etaSmoothGrave,
                etaRoughGrave,
                etaSmoothAcute,
                etaRoughAcute,
                etaSmoothCirc,
                etaRoughCirc,
                capEtaSmooth,
                capEtaRough,
                capEtaSmoothGrave,
                capEtaRoughGrave,
                capEtaSmoothAcute,
                capEtaRoughAcute,
                capEtaSmoothCirc,
                capEtaRoughCirc,
                iotaSmooth,
                iotaRough,
                iotaSmoothGrave,
                iotaRoughGrave,
                iotaSmoothAcute,
                iotaRoughAcute,
                iotaSmoothCirc,
                iotaRoughCirc,
                capIotaSmooth,
                capIotaRough,
                capIotaSmoothGrave,
                capIotaRoughGrave,
                capIotaSmoothAcute,
                capIotaRoughAcute,
                capIotaSmoothCirc,
                capIotaRoughCirc,
                omicronSmooth,
                omicronRough,
                omicronSmoothGrave,
                omicronRoughGrave,
                omicronSmoothAcute,
                omicronRoughAcute,
                capOmicronSmooth,
                capOmicronRough,
                capOmicronSmoothGrave,
                capOmicronRoughGrave,
                capOmicronSmoothAcute,
                capOmicronRoughAcute,
                upsilonSmooth,
                upsilonRough,
                upsilonSmoothGrave,
                upsilonRoughGrave,
                upsilonSmoothAcute,
                upsilonRoughAcute,
                upsilonSmoothCirc,
                upsilonRoughCirc,
                capUpsilonRough,
                capUpsilonRoughGrave,
                capUpsilonRoughAcute,
                capUpsilonRoughCirc,
                omegaSmooth,
                omegaRough,
                omegaSmoothGrave,
                omegaRoughGrave,
                omegaSmoothAcute,
                omegaRoughAcute,
                omegaSmoothCirc,
                omegaRoughCirc,
                capOmegaSmooth,
                capOmegaRough,
                capOmegaSmoothGrave,
                capOmegaRoughGrave,
                capOmegaSmoothAcute,
                capOmegaRoughAcute,
                capOmegaSmoothCirc,
                capOmegaRoughCirc,
                alphaGrave,
                alphaAcute,
                epsilonGrave,
                epsilonAcute,
                etaGrave,
                etaAcute,
                iotaGrave,
                iotaAcute,
                omicronGrave,
                omicronAcute,
                upsilonGrave,
                upsilonAcute,
                omegaGrave,
                omegaAcute,
                alphaSmoothIotaSub,
                alphaRoughIotaSub,
                alphaSmoothGraveIotaSub,
                alphaRoughGraveIotaSub,
                alphaSmoothAcuteIotaSub,
                alphaRoughAcuteIotaSub,
                alphaSmoothCircIotaSub,
                alphaRoughCircIotaSub,
                capAlphaSmoothIotaSub,
                capAlphaRoughIotaSub,
                capAlphaSmoothGraveIotaSub,
                capAlphaRoughGraveIotaSub,
                capAlphaSmoothAcuteIotaSub,
                capAlphaRoughAcuteIotaSub,
                capAlphaSmoothCircIotaSub,
                capAlphaRoughCircIotaSub,
                etaSmoothIotaSub,
                etaRoughIotaSub,
                etaSmoothGraveIotaSub,
                etaRoughGraveIotaSub,
                etaSmoothAcuteIotaSub,
                etaRoughAcuteIotaSub,
                etaSmoothCircIotaSub,
                etaRoughCircIotaSub,
                capEtaSmoothIotaSub,
                capEtaRoughIotaSub,
                capEtaSmoothGraveIotaSub,
                capEtaRoughGraveIotaSub,
                capEtaSmoothAcuteIotaSub,
                capEtaRoughAcuteIotaSub,
                capEtaSmoothCircIotaSub,
                capEtaRoughCircIotaSub,
                omegaSmoothIotaSub,
                omegaRoughIotaSub,
                omegaSmoothGraveIotaSub,
                omegaRoughGraveIotaSub,
                omegaSmoothAcuteIotaSub,
                omegaRoughAcuteIotaSub,
                omegaSmoothCircIotaSub,
                omegaRoughCircIotaSub,
                capOmegaSmoothIotaSub,
                capOmegaRoughIotaSub,
                capOmegaSmoothGraveIotaSub,
                capOmegaRoughGraveIotaSub,
                capOmegaSmoothAcuteIotaSub,
                capOmegaRoughAcuteIotaSub,
                capOmegaSmoothCircIotaSub,
                capOmegaRoughCircIotaSub,
                alphaBreve,
                alphaMacron,
                alphaGraveIotaSub,
                alphaIotaSub,
                alphaAcuteIotaSub,
                alphaCirc,
                alphaCircIotaSub,
                capAlphaBreve,
                capAlphaMacron,
                capAlphaGrave,
                capAlphaAcute,
                capAlphaIotaSub,
                etaGraveIotaSub,
                etaIotaSub,
                etaAcuteIotaSub,
                etaCirc,
                etaCircIotaSub,
                capEpsilonGrave,
                capEpsilonAcute,
                capEtaGrave,
                capEtaAcute,
                capEtaIotaSub,
                iotaBreve,
                iotaMacron,
                iotaCirc,
                capIotaBreve,
                capIotaMacron,
                capIotaGrave,
                capIotaAcute,
                upsilonBreve,
                upsilonMacron,
                rhoSmooth,
                rhoRough,
                upsilonCirc,
                capUpsilonBreve,
                capUpsilonMacron,
                capUpsilonGrave,
                capUpsilonAcute,
                capRhoRough,
                omegaGraveIotaSub,
                omegaIotaSub,
                omegaAcuteIotaSub,
                omegaCirc,
                omegaCircIotaSub,
                capOmicronGrave,
                capOmicronAcute,
                capOmegaGrave,
                capOmegaAcute,
                capOmegaIotaSub]
