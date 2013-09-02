{-# LANGUAGE OverloadedStrings #-}

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
import Data.Set (Set)
import qualified Data.Set as Set

import Control.Exceptional (Exceptional)
import qualified Control.Exceptional as Exc

import qualified Text.Parsec.Char as PCh
import Text.Parsec.Combinator
import Text.Parsec.Prim
import qualified Text.Parsec.String as PStr
import qualified Text.Parsec.Error as PErr
import Text.Parsec.Pos

import Data.Greek.Texty
import Data.Greek.Word
import Data.Greek.Normalize
import Data.Greek.UnicodeData

-- XXX offset may not be useful, given automatic normalization.

-- XXX client no longer needs to normalize text.

-- XXX probably want to signal error on trailing input.

-- | Signals an error in converting text to greek representation.
data ParseError = ParseError { offset :: Int,
                               -- ^ Offset within _normalized_ string at which
                               --   error occurred
                               messages :: [String]
                               -- ^ Non-empty list of parse error messages.
                             }
                deriving (Eq)

type Parser result = PStr.GenParser Char () result

instance Show ParseError where
  show (ParseError offset msgs) =
    Prelude.concat ["Greek parse error at position ",
                    show offset,
                    ": ",
                    List.intercalate ", " msgs]

-- | Converts the provided 'Texty' value into a Greek 'Word'.  Parses as much
--   of the string as possible, but ignores trailing input.
word :: Texty a => a -> Exceptional ParseError Word
word = parsecWrapper parseWord "word"

-- | Converts the provided 'Texty' value into a Greek 'Letter'.  Ignores any
--   extra input after that which defines the first letter.
letter :: Texty a => a -> Exceptional ParseError Letter
letter = parsecWrapper parseLetter "letter"

-- | Convert a Parsec parser into a function suitable for export.
parsecWrapper :: Texty a =>
                 Parser b -> String -> a -> Exceptional ParseError b
parsecWrapper parser sourceLabel src =
  case parse parser sourceLabel (unpack (normalize src)) of
    Left err ->
      Exc.throw (ParseError (sourceLine (PErr.errorPos err))
                 (map PErr.messageString (PErr.errorMessages err)))
    Right result -> return result

-- | Normalize the input and then attempt to parse it as a 'Word'.  Aborts on
--   parse errors, so this is intended for expressing literal 'Word's in code,
--   and not for parsing user input.
literalWord :: Texty a => a -> Word
literalWord src = Exc.run' (word (normalize src))

-- | Normalize the input and then parse it as a 'Letter'.  As with
--   'literalWord', this aborts on parse errors, so use only for literal
--   'Letter's in code.
literalLetter :: Texty a => a -> Letter
literalLetter src = Exc.run' (letter (normalize src))

-- | Set of those base characters that denote vowels.
baseVowels :: Set Char
baseVowels =
  Set.fromList
  [baseAlpha, baseEpsilon, baseEta, baseIota, baseOmicron, baseUpsilon,
   baseOmega, capAlpha, capEpsilon, capEta, capIota, capOmicron, capUpsilon,
   capOmega]

-- | Set of those base characters that denote consonants.
baseConsonants :: Set Char
baseConsonants = Set.difference baseChars baseVowels

-- | Set of those base characters that denote consonants, without rho.
nonRhoConsonants :: Set Char
nonRhoConsonants =
  Set.difference baseConsonants (Set.fromList [baseRho, capRho])

-- | Set of characters that denote Greek combining diacriticals.
combiningDiacriticals :: Set Char
combiningDiacriticals =
  Set.fromList [combSmooth, combRough,
                combAcute, combGrave, combCirc,
                combDialytika,
                combIotaSub]

-- | Parse a single 'Word' from the input.
parseWord :: Parser Word
parseWord =
  (do letters <- many1 (try parseLetter)
      return $ makeWord letters)
  <?> "greek word"

-- | Parse a single 'Letter' from the input, including leading underscore and
--   trailing combining diacriticals, if appropriate.
parseLetter :: Parser Letter
parseLetter =
  (consonant
   <|> try alpha
   <|> epsilon
   <|> eta
   <|> try iota
   <|> omicron
   <|> try upsilon
   <|> omega)
  <?> "greek letter"

consonant :: Parser Letter
consonant =
  ((do baseChar <- PCh.oneOf (Set.elems nonRhoConsonants)
       notFollowedBy diacritical
       return $ makeLetter baseChar NoBreathing NoAccent NoIotaSub NoMacron)
   <|>
   (do rho <- PCh.oneOf [baseRho, capRho]
       br <- optBreathing
       notFollowedBy diacritical
       return $ makeLetter rho br NoAccent NoIotaSub NoMacron))
  <?> "greek consonant"

alpha :: Parser Letter
alpha =
  (do PCh.char '_'
      alpha <- PCh.oneOf [baseAlpha, capAlpha]
      br <- optBreathing
      ac <- optAccentAfterMacron
      iotaSub <- optIotaSub
      when (iotaSub == IotaSub)
        (fail "iota subscript and macron may not occur together")
      notFollowedBy diacritical
      return $ makeLetter alpha br ac NoIotaSub Macron)
  <|> (do alpha <- PCh.oneOf [baseAlpha, capAlpha]
          br <- optBreathing
          ac <- optAccent
          iotaSub <- optIotaSub
          notFollowedBy diacritical
          return $ makeLetter alpha br ac iotaSub NoMacron)

epsilon :: Parser Letter
epsilon =
  do epsilon <- PCh.oneOf [baseEpsilon, capEpsilon]
     br <- optBreathing
     ac <- optAccNoCirc
     notFollowedBy diacritical
     return $ makeLetter epsilon br ac NoIotaSub NoMacron

eta :: Parser Letter
eta =
  do eta <- PCh.oneOf [baseEta, capEta]
     br <- optBreathing
     ac <- optAccent
     iotaSub <- optIotaSub
     notFollowedBy diacritical
     return $ makeLetter eta br ac iotaSub NoMacron

iota :: Parser Letter
iota =
  (do PCh.char '_'
      iota <- PCh.oneOf [baseIota, capIota]
      br <- optBreathing
      ac <- optAccentAfterMacron
      notFollowedBy diacritical
      return $ makeLetter iota br ac NoIotaSub Macron)
  <|> (do iota <- PCh.oneOf [baseIota, capIota]
          br <- optBreathing
          ac <- optAccent
          notFollowedBy diacritical
          return $ makeLetter iota br ac NoIotaSub NoMacron)

omicron :: Parser Letter
omicron =
  do omicron <- PCh.oneOf [baseOmicron, capOmicron]
     br <- optBreathing
     ac <- optAccNoCirc
     notFollowedBy diacritical
     return $ makeLetter omicron br ac NoIotaSub NoMacron

upsilon :: Parser Letter
upsilon =
  (do PCh.char '_'
      upsilon <- PCh.oneOf [baseUpsilon, capUpsilon]
      br <- optBreathing
      ac <- optAccentAfterMacron
      notFollowedBy diacritical
      return $ makeLetter upsilon br ac NoIotaSub Macron)
  <|> (do upsilon <- PCh.oneOf [baseUpsilon, capUpsilon]
          br <- optBreathing
          ac <- optAccent
          notFollowedBy diacritical
          return $ makeLetter upsilon br ac NoIotaSub NoMacron)

omega :: Parser Letter
omega =
  do omega <- PCh.oneOf [baseOmega, capOmega]
     br <- optBreathing
     ac <- optAccent
     iotaSub <- optIotaSub
     notFollowedBy diacritical
     return $ makeLetter omega br ac iotaSub NoMacron

optBreathing :: Parser Breathing
optBreathing =
  (PCh.char combSmooth >> return Smooth)
  <|> (PCh.char combRough >> return Rough)
  <|> return NoBreathing

optAccNoCirc :: Parser Accent
optAccNoCirc =
  (PCh.char combAcute >> return Acute)
  <|> (PCh.char combGrave >> return Grave)
  <|> return NoAccent

optAccent :: Parser Accent
optAccent =
  (PCh.char combAcute >> return Acute)
  <|> (PCh.char combGrave >> return Grave)
  <|> (PCh.char combCirc >> return Circumflex)
  <|> return NoAccent

-- | Parse an optional combining accent on a vowel with an explicit macron.
--   Behaves like 'optAccNoCirc', but fails with a detailed error message
--   upon parsing a circumflex.
optAccentAfterMacron :: Parser Accent
optAccentAfterMacron =
  do a <- optAccent
     when (a == Circumflex)
       (fail "circumflex and macron may not occur together")
     return a

optIotaSub :: Parser IotaSub
optIotaSub =
  (PCh.char combIotaSub >> return IotaSub)
  <|> return NoIotaSub

diacritical :: Parser Char
diacritical =
  (PCh.oneOf (Set.elems combiningDiacriticals))
  <?> "combining diacritical"
