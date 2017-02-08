{-# LANGUAGE ScopedTypeVariables #-}

module GreekLetterGen(
  PrefixWords(..),
  IotaSubWords(..),
  BreathingWords(..),
  AccentWords(..),
  MacronWords(..),
  CaseWords(..)
  )
where

import Prelude hiding (Word)
import qualified Data.Char as Char
import qualified Data.Set as Set
import Test.QuickCheck
import Data.Greek.Word
import Data.Greek.UnicodeData

-- | Concrete representation of 'Letter', for ease of update by generators.
data LetterProps = LetterProps { base :: Char,
                                 iotaSub :: IotaSub,
                                 breathing :: Breathing,
                                 accent :: Accent,
                                 macron :: Macron }
                   deriving Show

-- | Contains two 'Word' values; the first is a prefix of the second.
data PrefixWords = PrefixWords Word Word
                 deriving Show

-- | Contains two 'Word' values whose base letters are identical but may vary
--   all other properties (iota subscript, breathing, accent, macron, and
--   case).
data IotaSubWords = IotaSubWords Word Word
                  deriving Show

-- | Contains two 'Word' values that differ only in breathing, accent, macron,
--   and case.
data BreathingWords = BreathingWords Word Word
                      deriving Show

-- | Contains two 'Word' values that differ only in accent, macron, and case.
data AccentWords = AccentWords Word Word
                 deriving Show

-- | Contains two 'Word' values that differ only in macron and case.
data MacronWords = MacronWords Word Word
                 deriving Show

-- | Contains two 'Word' values that differ only in case.
data CaseWords = CaseWords Word Word
               deriving Show
-- XXX may want Show instance for CaseWords that prints out unicode rendering,
-- English, and hex escapes, or something.

instance Arbitrary Word where
  -- | Generate an arbitrary 'Word' of 'Letter' values.  Word-level invariants
  -- (e.g., breathing only on initial letter, final sigma only at end) are not
  -- guaranteed, but each individual letter is guaranteed to be valid.
  arbitrary =
    do (letters :: [Letter]) <- listOf1 arbitrary
       return $ makeWord letters

instance Arbitrary Letter where
  arbitrary =
    do (LetterProps base iotaSub breathing accent macron) <-
         arbitrary `suchThat` validProps
       return $ makeLetter base breathing accent iotaSub macron

instance Arbitrary LetterProps where
  -- | Generate an arbitrary, not necessarily valid, combination of letter
  --   properties.
  arbitrary =
    do base <- elements (Set.elems baseChars)
       iotaSub <- elements [NoIotaSub, IotaSub]
       breathing <- elements [NoBreathing, Smooth, Rough]
       accent <- elements [NoAccent, Acute, Grave, Circumflex]
       macron <- elements [NoMacron, Macron]
       return $ LetterProps base iotaSub breathing accent macron

instance Arbitrary PrefixWords where
  arbitrary =
    do w <- arbitrary
       let letters = getLetters w
       prefixSize <- elements [1..length letters]
       return $ PrefixWords (makeWord (take prefixSize letters)) w

instance Arbitrary IotaSubWords where
  arbitrary =
    do (BreathingWords w1 w2) <- arbitrary
       w2' <- updateWord w2 updateIotaSub [NoIotaSub, IotaSub]
       return (IotaSubWords w1 w2')

instance Arbitrary BreathingWords where
  arbitrary =
    do (AccentWords w1 w2) <- arbitrary
       w2' <- updateWord w2 updateBreathing [NoBreathing, Rough, Smooth]
       return (BreathingWords w1 w2')

instance Arbitrary AccentWords where
  arbitrary =
    do (MacronWords w1 w2) <- arbitrary
       w2' <- updateWord w2 updateAccent [NoAccent .. Circumflex]
       return (AccentWords w1 w2')

instance Arbitrary MacronWords where
  arbitrary =
    do (CaseWords w1 w2) <- arbitrary
       w2' <- updateWord w2 updateMacron [NoMacron, Macron]
       return (MacronWords w1 w2')

instance Arbitrary CaseWords where
  arbitrary =
    do w1 <- arbitrary
       w2 <- updateWord w1 updateCase [toLowerExceptSigma, toUpperExceptSigma]
       return (CaseWords w1 w2)
  shrink (CaseWords w1 w2) =
    case (getLetters w1, getLetters w2) of
      ([], []) -> []
      (x:xs, y:ys) -> [CaseWords (makeWord xs) (makeWord ys)]
      _ -> error "CaseWords shrink"

-- | Recognizes a valid combination of letter properties.
validProps :: LetterProps -> Bool
validProps (LetterProps base iotaSub breathing accent macron) =
  validLetter macron base breathing accent iotaSub

-- | @updateWord (makeWord [l1, ..., l_n]) updateProp values@ is a generator
--   that produces @makeWord [updateProp l1 v1, ..., updateProp l_n v_n]@,
--   where the @v_i@s are generated from the @values@ list independently for
--   each letter.
updateWord :: Word -> (LetterProps -> a -> LetterProps) -> [a] -> Gen Word
updateWord baseWord updateProp values =
  do letters <- mapM (updateLetter updateProp values) (getLetters baseWord)
     return (makeWord letters)

-- | @updateLetter updateProp values baseLetter@ is a generator that produces
--   @updateProp baseLetter val@, where @val@ is a value generated from the
--   @values@ list.
updateLetter :: (LetterProps -> a -> LetterProps) -> [a] -> Letter
                -> Gen Letter
updateLetter updateProp values baseLetter =
  let baseProps = letterToProps baseLetter
      newPropIsValid p = validProps (updateProp baseProps p)
  in
   do newProp <- elements values `suchThat` newPropIsValid
      return (propsToLetter (updateProp baseProps newProp))

updateIotaSub :: LetterProps -> IotaSub -> LetterProps
updateIotaSub props iota = props { iotaSub = iota }

updateBreathing :: LetterProps -> Breathing -> LetterProps
updateBreathing props b = props { breathing = b }

updateAccent :: LetterProps -> Accent -> LetterProps
updateAccent p a = p { accent = a }

updateMacron :: LetterProps -> Macron -> LetterProps
updateMacron props m = props { macron = m }

updateCase :: LetterProps -> (Char -> Char) -> LetterProps
updateCase props f = props { base = f (base props) }

-- | Converts all letters except sigma to lowercase, if not already lowercase.
toLowerExceptSigma :: Char -> Char
toLowerExceptSigma l =
  if l == capSigma then l else Char.toLower l

-- | Converts all letters except sigma to uppercase, if not uppercase already.
toUpperExceptSigma :: Char -> Char
toUpperExceptSigma l =
  if l == baseFinalSigma || l == baseMedialSigma then l else Char.toUpper l

letterToProps :: Letter -> LetterProps
letterToProps l = LetterProps { base = getBase l,
                                accent = getAccent l,
                                breathing = getBreathing l,
                                iotaSub = getIotaSub l,
                                macron = getMacron l }

propsToLetter :: LetterProps -> Letter
propsToLetter (LetterProps base iotaSub breathing accent macron) =
  makeLetter base breathing accent iotaSub macron
