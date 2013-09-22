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

import Data.Greek.Texty (Texty)
import qualified Data.Greek.Texty as Texty

import Data.Greek.Word
import Data.Greek.UnicodeData

-- | Translates a 'Letter' to input syntax.
letterToUnicode :: Texty a => Letter -> a
letterToUnicode l =
  unicodeRenderer prependMacron l

-- | Translates a 'Word' to input syntax.
wordToUnicode :: Texty a => Word -> a
wordToUnicode wd =
  Texty.concat (map letterToUnicode (getLetters wd))

-- | Translates a 'Letter' to LaTeX syntax.
letterToLaTeX :: Texty a => Letter -> a
letterToLaTeX l =
  unicodeRenderer addLatexMacron l

-- | Translates a 'Word' to LaTeX syntax.
wordToLaTeX :: Texty a => Word -> a
wordToLaTeX wd =
  Texty.concat (map letterToLaTeX (getLetters wd))

-- | Converts a 'Letter' to its normalized representation.
unicodeRenderer :: Texty a
                   => (a -> a)  -- ^ function to add macron
                   -> Letter    -- ^ letter to convert
                   -> a
unicodeRenderer addMacron l =
  (case getMacron l of
      NoMacron -> id
      Macron -> addMacron)
  (Texty.concat [Texty.singleton (getBase l),
                 case getBreathing l of
                   NoBreathing -> Texty.fromString ""
                   Smooth -> Texty.singleton combSmooth
                   Rough -> Texty.singleton combRough,
                 case getAccent l of
                   NoAccent -> Texty.fromString ""
                   Acute -> Texty.singleton combAcute
                   Grave -> Texty.singleton combGrave
                   Circumflex -> Texty.singleton combCirc,
                 case getIotaSub l of
                   NoIotaSub -> Texty.fromString ""
                   IotaSub -> Texty.singleton combIotaSub])

prependMacron :: Texty a => a -> a
prependMacron t = Texty.cons '_' t

addLatexMacron :: Texty a => a -> a
addLatexMacron t =
  Texty.concat [Texty.fromString "\\_{", t, Texty.singleton '}']
