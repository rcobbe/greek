{-# LANGUAGE OverloadedStrings #-}

-- | Parses external representations of Greek Text.  The input should be
--   Unicode text, containing only Greek letters and combining diacriticals, in
--   the format defined by 'Data.Greek.Normalize.normalize'.  We also allow
--   underscores immediately before certain vowels, to indicate that the vowel
--   is long.  (The underscore corresponds to the presence of a macron in the
--   written representation, and not to vowel length -- so, for instance,
--   \"_ά\" denotes a long alpha with an acute accent, but \"_ᾶ\" results in a
--   parse error, because the macron is redundant with the circumflex.)
module Data.Greek.Parser(ParseException(..),
                         word,
                         literalWord,
                         letter) where

import Control.Monad
import qualified Data.Set as Set
import qualified Data.Text.Lazy as Text
import Data.Trie (Trie)
import qualified Data.Trie as Trie

import Data.Greek.Normalize
import Data.Greek.Texty (Texty)
import qualified Data.Greek.Texty as Texty
import Data.Greek.Word
import Data.Greek.Output

-- XXX ditch use of Trie.  First split normalized input into characters (opt
-- macron, base, breathing, diaeresis, accent, iota sub), then parse individual
-- letters.  This lets us give more useful error messages when on, say, alpha
-- plus macron and circumflex.  (Trie just reports invalid input on circumflex.)

-- | Exception used to signal invalid Greek input to one of the parsing
--   routines.  The String value is the bad input.
data ParseException = InvalidInputException String
                    deriving (Eq, Show)

-- | Normalizes the input and then attempts to parse it as a 'Word'.  Returns a
--   'ParseException' if the input isn't a valid 'Word'; this includes
--   extraneous trailing input.
word :: Texty a => a -> Either ParseException Word
word input =
  let normalizedInput = normalize input
  in
   do when (Texty.null normalizedInput) (Left $ InvalidInputException "")
      letters <- parseLetters (Texty.unpack normalizedInput)
      return (makeWord letters)

-- | Normalize the input and parse it as a 'Word'.  Aborts on parse errors
--   (including extraneous trailing input), so this is best used for expressing
--   literal Words in code and not for parsing user input.
literalWord :: Texty a => a -> Word
literalWord input =
  case word input of
    Left (InvalidInputException s) ->
      error $ "literalWord: invalid input: " ++ s
    Right w -> w

-- | Normalize the input and parse it as a single letter.  Returns a
--   'ParseException' on parse errors, including trailing input.
letter :: Texty a => a -> Either ParseException Letter
letter input =
  do w <- word input
     when (length (getLetters w) /= 1)
       (Left $ InvalidInputException (Texty.unpack (normalize input)))
     return (head (getLetters w))

-- | Main parse loop.  Attempts to parse the *entire* argument list, which must
--   be normalized, as a sequence of Greek letters.  Throws a 'ParseException'
--   if the input isn't valid.
parseLetters :: [Char] -> Either ParseException [Letter]
parseLetters [] = return []
parseLetters input =
  case Trie.matchPrefix inputTrie input of
    Just (newInput, letter) ->
      do remainingLetters <- parseLetters newInput
         return (letter : remainingLetters)
    Nothing ->
      Left $ InvalidInputException input

-- | Trie defining input from sequences of 'Char' to individual Greek
--   'Letter's.
inputTrie :: Trie Char Letter
inputTrie =
  Trie.fromList
    [(Text.unpack (letterToUnicode letter), letter) |
     base <- Set.elems baseChars,
     iotaSub <- [NoIotaSub, IotaSub],
     breathing <- [NoBreathing, Smooth, Rough],
     accent <- [NoAccent, Acute, Grave, Circumflex],
     macron <- [NoMacron, Macron],
     validLetter macron base breathing accent iotaSub,
     let letter = makeLetter base breathing accent iotaSub macron]

