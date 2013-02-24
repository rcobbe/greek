{-# LANGUAGE OverloadedStrings #-}

-- | Defines routines for rendering Greek 'Letter's and 'Word's into 'String's,
--   either in the input syntax (with leading underscore to indicate long
--   vowels) or in LaTeX syntax (with leading backslash and underscore to
--   indicate long vowels).  Both syntaxes are rendered in normalized order;
--   see "Data.Greek.Normalize" for details.

module Data.Greek.Output(
  letterToUnicode,
  wordToUnicode,
  letterToLaTeX,
  wordToLaTeX)
where

import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as Text

import Data.Greek.Word
import Data.Greek.UnicodeData

-- | Translates a 'Letter' to input syntax.
letterToUnicode :: Letter -> Text
letterToUnicode l =
  unicodeRenderer prependMacron l

-- | Translates a 'Word' to input syntax.
wordToUnicode :: Word -> Text
wordToUnicode wd =
  Text.concat (map letterToUnicode (getLetters wd))

-- | Translates a 'Letter' to LaTeX syntax.
letterToLaTeX :: Letter -> Text
letterToLaTeX l =
  unicodeRenderer addLatexMacron l

-- | Translates a 'Word' to LaTeX syntax.
wordToLaTeX :: Word -> Text
wordToLaTeX wd =
  Text.concat (map letterToLaTeX (getLetters wd))

-- | Converts a 'Letter' to its normalized representation.
unicodeRenderer :: (Text -> Text) -- ^ function to add macron
                   -> Letter    -- ^ letter to convert
                   -> Text
unicodeRenderer addMacron l =
  (case getMacron l of
      NoMacron -> id
      Macron -> addMacron)
  (Text.concat [Text.singleton (getBase l),
                case getBreathing l of
                  NoBreathing -> ""
                  Smooth -> Text.singleton combSmooth
                  Rough -> Text.singleton combRough,
                case getAccent l of
                  NoAccent -> ""
                  Acute -> Text.singleton combAcute
                  Grave -> Text.singleton combGrave
                  Circumflex -> Text.singleton combCirc,
                case getIotaSub l of
                  NoIotaSub -> ""
                  IotaSub -> Text.singleton combIotaSub])

prependMacron :: Text -> Text
prependMacron t = Text.append "_" t

addLatexMacron :: Text -> Text
addLatexMacron t = Text.concat ["\\_{", t, "}"]
