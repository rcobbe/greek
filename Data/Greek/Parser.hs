{-# LANGUAGE OverloadedStrings #-}

-- | Parses external representations of Greek Text.  The input should be
--   Unicode text, containing only Greek letters and combining diacriticals, in
--   the format defined by 'Data.Greek.Normalize.normalize'.  We also allow
--   underscores immediately before certain vowels, to indicate that the vowel
--   is long.  (The underscore corresponds to the presence of a macron in the
--   written representation, and not to vowel length -- so, for instance,
--   \"_ά\" denotes a long alpha with an acute accent, but \"_ᾶ\" results in a
--   parse error, because the macron is redundant with the circumflex.)
module Data.Greek.Parser(word,
                         letter,
                         literalWord,
                         literalLetter) where

-- XXX remove "g" prefix on function names

import Control.Monad
import Data.Set (Set)
import qualified Data.Set as Set

import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as Text

import qualified Text.Parsec.Char as PCh
import Text.Parsec.Combinator
import Text.Parsec.Prim
import Text.Parsec.Text.Lazy

import Data.Greek.Word
import Data.Greek.Normalize
import Data.Greek.UnicodeData

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
word :: GenParser st Word
word =
  (do letters <- many1 (try letter)
      return $ makeWord letters)
  <?> "greek word"

-- | Parse a single 'Letter' from the input, including leading underscore and
--   trailing combining diacriticals, if appropriate.
letter :: GenParser st Letter
letter =
  (gConsonant
   <|> try gAlpha
   <|> gEpsilon
   <|> gEta
   <|> try gIota
   <|> gOmicron
   <|> try gUpsilon
   <|> gOmega)
  <?> "greek letter"

gConsonant :: GenParser st Letter
gConsonant =
  ((do baseChar <- PCh.oneOf (Set.elems nonRhoConsonants)
       notFollowedBy diacritical
       return $ makeLetter baseChar NoBreathing NoAccent NoIotaSub NoMacron)
   <|>
   (do rho <- PCh.oneOf [baseRho, capRho]
       br <- optBreathing
       notFollowedBy diacritical
       return $ makeLetter rho br NoAccent NoIotaSub NoMacron))
  <?> "greek consonant"

gAlpha :: GenParser st Letter
gAlpha =
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

gEpsilon :: GenParser st Letter
gEpsilon =
  do epsilon <- PCh.oneOf [baseEpsilon, capEpsilon]
     br <- optBreathing
     ac <- optAccNoCirc
     notFollowedBy diacritical
     return $ makeLetter epsilon br ac NoIotaSub NoMacron

gEta :: GenParser st Letter
gEta =
  do eta <- PCh.oneOf [baseEta, capEta]
     br <- optBreathing
     ac <- optAccent
     iotaSub <- optIotaSub
     notFollowedBy diacritical
     return $ makeLetter eta br ac iotaSub NoMacron

gIota :: GenParser st Letter
gIota =
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

gOmicron :: GenParser st Letter
gOmicron =
  do omicron <- PCh.oneOf [baseOmicron, capOmicron]
     br <- optBreathing
     ac <- optAccNoCirc
     notFollowedBy diacritical
     return $ makeLetter omicron br ac NoIotaSub NoMacron

gUpsilon :: GenParser st Letter
gUpsilon =
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

gOmega :: GenParser st Letter
gOmega =
  do omega <- PCh.oneOf [baseOmega, capOmega]
     br <- optBreathing
     ac <- optAccent
     iotaSub <- optIotaSub
     notFollowedBy diacritical
     return $ makeLetter omega br ac iotaSub NoMacron

optBreathing :: GenParser st Breathing
optBreathing =
  (PCh.char combSmooth >> return Smooth)
  <|> (PCh.char combRough >> return Rough)
  <|> return NoBreathing

optAccNoCirc :: GenParser st Accent
optAccNoCirc =
  (PCh.char combAcute >> return Acute)
  <|> (PCh.char combGrave >> return Grave)
  <|> return NoAccent

optAccent :: GenParser st Accent
optAccent =
  (PCh.char combAcute >> return Acute)
  <|> (PCh.char combGrave >> return Grave)
  <|> (PCh.char combCirc >> return Circumflex)
  <|> return NoAccent

-- | Parse an optional combining accent on a vowel with an explicit macron.
--   Behaves like 'optAccNoCirc', but fails with a detailed error message
--   upon parsing a circumflex.
optAccentAfterMacron :: GenParser st Accent
optAccentAfterMacron =
  do a <- optAccent
     when (a == Circumflex)
       (fail "circumflex and macron may not occur together")
     return a

optIotaSub :: GenParser st IotaSub
optIotaSub =
  (PCh.char combIotaSub >> return IotaSub)
  <|> return NoIotaSub

diacritical :: GenParser st Char
diacritical =
  (PCh.oneOf (Set.elems combiningDiacriticals))
  <?> "combining diacritical"

-- | Normalize the input and then attempt to parse it as a 'Word'.  Aborts on
--   parse errors, so this is intended for expressing literal 'Word's in code,
--   and not for parsing user input.
literalWord :: Text -> Word
literalWord = makeLiteralParser word

-- | Normalize the input and then parse it as a 'Letter'.  As with
--   'literalWord', this aborts on parse errors, so use only for literal
--   'Letter's in code.
literalLetter :: Text -> Letter
literalLetter = makeLiteralParser letter

makeLiteralParser :: GenParser () a -> Text -> a
makeLiteralParser p str =
  let pPlusEof = do result <- p
                    eof
                    return result
  in case parse pPlusEof "" (normalize str) of
    Left err -> error (show err)
    Right w -> w

