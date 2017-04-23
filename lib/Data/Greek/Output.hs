{-# LANGUAGE OverloadedStrings #-}

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

-- | Defines routines for producing textual representations of Greek 'Letter's
--   and 'Word's, either in the input syntax (with leading underscore to
--   indicate long vowels) or in LaTeX syntax (using the underscore macro to
--   indicate long vowels).  Both syntaxes are rendered in normalized order;
--   see "Data.Greek.Normalize" for details.

module Data.Greek.Output(
  letterToUnicode,
  wordToUnicode,
  letterToLaTeX,
  wordToLaTeX)
where

import Prelude hiding (Word)
import Data.Textual (Textual)
import qualified Data.Textual as Textual

import Data.Greek.Word
import Data.Greek.UnicodeData

-- | Translates a 'Letter' to input syntax.
letterToUnicode :: Textual a => Letter -> a
letterToUnicode l =
  unicodeRenderer prependMacron l

-- | Translates a 'Word' to input syntax.
wordToUnicode :: Textual a => Word -> a
wordToUnicode wd =
  Textual.concat (map letterToUnicode (getLetters wd))

-- | Translates a 'Letter' to LaTeX syntax.
letterToLaTeX :: Textual a => Letter -> a
letterToLaTeX l =
  unicodeRenderer addLatexMacron l

-- | Translates a 'Word' to LaTeX syntax.
wordToLaTeX :: Textual a => Word -> a
wordToLaTeX wd =
  Textual.concat (map letterToLaTeX (getLetters wd))

-- | Converts a 'Letter' to its normalized representation.
unicodeRenderer :: Textual a
                   => (a -> a)  -- ^ function to add macron
                   -> Letter    -- ^ letter to convert
                   -> a
unicodeRenderer addMacron l =
  (case getMacron l of
      NoMacron -> id
      Macron -> addMacron)
  (Textual.concat [Textual.singleton (getBase l),
                   case getBreathing l of
                     NoBreathing -> Textual.empty
                     Smooth -> Textual.singleton combSmooth
                     Rough -> Textual.singleton combRough,
                   case getAccent l of
                     NoAccent -> Textual.empty
                     Acute -> Textual.singleton combAcute
                     Grave -> Textual.singleton combGrave
                     Circumflex -> Textual.singleton combCirc,
                   case getIotaSub l of
                     NoIotaSub -> Textual.empty
                     IotaSub -> Textual.singleton combIotaSub])

prependMacron :: Textual a => a -> a
prependMacron t = Textual.cons '_' t

addLatexMacron :: Textual a => a -> a
addLatexMacron t =
  Textual.concat [Textual.fromString "\\_{", t, Textual.singleton '}']
