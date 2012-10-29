{-# LANGUAGE OverloadedStrings #-}

module Data.Greek.Parser(word,
                         letter,
                         literalWord,
                         literalLetter) where

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

baseVowels :: Set Char
baseVowels =
  Set.fromList
  [baseAlpha, baseEpsilon, baseEta, baseIota, baseOmicron, baseUpsilon,
   baseOmega, capAlpha, capEpsilon, capEta, capIota, capOmicron, capUpsilon,
   capOmega]

baseConsonants :: Set Char
baseConsonants = Set.difference baseChars baseVowels

nonRhoConsonants :: Set Char
nonRhoConsonants =
  Set.difference baseConsonants (Set.fromList [baseRho, capRho])

combiningDiacriticals :: Set Char
combiningDiacriticals =
  Set.fromList [combSmooth, combRough,
                combAcute, combGrave, combCirc,
                combDialytika,
                combIotaSub]

word :: GenParser st Word
word =
  (do letters <- many1 (try letter)
      return $ makeWord letters)
  <?> "greek word"

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
      ac <- optAccNoCirc
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
      ac <- optAccNoCirc
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
      ac <- optAccNoCirc
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

optIotaSub :: GenParser st IotaSub
optIotaSub =
  (PCh.char combIotaSub >> return IotaSub)
  <|> return NoIotaSub

diacritical :: GenParser st Char
diacritical = PCh.oneOf (Set.elems combiningDiacriticals)

literalWord :: Text -> Word
literalWord = makeLiteralParser word

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

