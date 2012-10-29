module Data.Greek.Word(
  -- * Types
  Word,
  getLetters,
  Letter,
  getBase,
  getMacron,
  getBreathing,
  getAccent,
  getIotaSub,
  Macron(..),
  Breathing(..),
  Accent(..),
  IotaSub(..),
  -- * Constructors
  makeLetter,
  makeWord,
  validLetter,
  baseChars
  )
where

import Data.List
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

-- | Represents a word of Greek.
data Word = Word [Letter]
           deriving (Eq)

getLetters :: Word -> [Letter]
getLetters (Word xs) = xs

-- | Representation of a single Greek letter.
data Letter = Letter {
  -- | Greek letter, with no diacriticals.  Must be a Unicode character in one
  --   of the ranges \\x0391-\\x03a9, \\x03b1-\\x03c9, or \\x03dc-\\x03dd.
  base :: Char,
  -- | Does the letter have an iota subscript?  Valid only when base is alpha,
  --   eta, or omega, and the letter does not have a macron.
  iotaSub :: IotaSub,
  -- | Does the letter have a breathing mark?  Valid only on vowels or rho.  We
  --   allow smooth breathing on both rho and upsilon for situations like
  --   Πυῤῥος and οὐτός.  We also do not ensure that breathing marks appear
  --   only at the beginning of the word.
  breathing :: Breathing,
  -- | Does the letter have an accent?  Valid only on vowels; circumflexes
  --   valid only on alpha, eta, iota, upsilon, omega, and only when the letter
  --   does not have a macron.
  accent :: Accent,
  -- | Does the letter need an explicit macron for its Unicode/LaTeX
  --   representation?  Valid only on alpha, iota, upsilon, and only when the
  --   letter has neither a circumflex nor a iota subscript.  We don't enforce
  --   the invariant that only monophthongs may have macrons.
  --
  --   We considered representing vowel length rather than the presence of a
  --   macron, as then we could use the same representation for Polyglot and
  --   Lexicon.  However, the existence of diphthongs makes it difficult to
  --   determine vowel length in isolation.
  macron :: Macron }
            deriving (Eq, Ord)

-- | Does a letter have an iota subscript?
data IotaSub = NoIotaSub | IotaSub
             deriving (Eq, Show, Ord, Enum)

-- | Does a letter have a breathing mark?
data Breathing = NoBreathing | Smooth | Rough
               deriving (Eq, Show, Ord, Enum)

-- | Does a letter have an accent?
data Accent = NoAccent | Acute | Grave | Circumflex
            deriving (Eq, Show, Ord, Enum)

-- | Does a letter require an explicit macron?
data Macron = NoMacron | Macron
            deriving (Eq, Show, Ord, Enum)

getBase :: Letter -> Char
getBase = base

getMacron :: Letter -> Macron
getMacron = macron

getBreathing :: Letter -> Breathing
getBreathing = breathing

getAccent :: Letter -> Accent
getAccent = accent

getIotaSub :: Letter -> IotaSub
getIotaSub = iotaSub

instance Show Word where
  show (Word letters) =
    ("Word [" ++ intercalate ", " (map show letters) ++ "]")

instance Show Letter where
  show (Letter base iotaSub breathing accent macron) =
    intercalate " "
    (filter (not . null) [renderBreathing breathing,
                          renderAccent accent,
                          renderLength macron,
                          renderBase base,
                          renderIotaSub iotaSub])

renderBreathing :: Breathing -> String
renderBreathing NoBreathing = ""
renderBreathing Smooth = "smooth"
renderBreathing Rough = "rough"

renderAccent :: Accent -> String
renderAccent NoAccent = ""
renderAccent Acute = "acute"
renderAccent Grave = "grave"
renderAccent Circumflex = "circumflex"

renderLength :: Macron -> String
renderLength NoMacron = ""
renderLength Macron = "long"

renderIotaSub :: IotaSub -> String
renderIotaSub NoIotaSub = ""
renderIotaSub IotaSub = "iota-sub"

renderBase :: Char -> String
renderBase c = letterTable ! c
  where letterTable :: Map Char String
        letterTable =
          Map.fromList [('α', "alpha"),
                        ('β', "beta"),
                        ('γ', "gamma"),
                        ('δ', "delta"),
                        ('ε', "epsilon"),
                        ('ϝ', "digamma"),
                        ('ζ', "zeta"),
                        ('η', "eta"),
                        ('θ', "theta"),
                        ('ι', "iota"),
                        ('κ', "kappa"),
                        ('λ', "lambda"),
                        ('μ', "mu"),
                        ('ν', "nu"),
                        ('ξ', "xi"),
                        ('ο', "omicron"),
                        ('π', "pi"),
                        ('ρ', "rho"),
                        ('σ', "sigma"),
                        ('ς', "final sigma"),
                        ('τ', "tau"),
                        ('υ', "upsilon"),
                        ('φ', "phi"),
                        ('χ', "chi"),
                        ('ψ', "psi"),
                        ('ω', "omega"),
                        ('Α', "Alpha"),
                        ('Β', "Beta"),
                        ('Γ', "Gamma"),
                        ('Δ', "Delta"),
                        ('Ε', "Epsilon"),
                        ('Ϝ', "Digamma"),
                        ('Ζ', "Zeta"),
                        ('Η', "Eta"),
                        ('Θ', "Theta"),
                        ('Ι', "Iota"),
                        ('Κ', "Kappa"),
                        ('Λ', "Lambda"),
                        ('Μ', "Mu"),
                        ('Ν', "Nu"),
                        ('Ξ', "Xi"),
                        ('Ο', "Omicron"),
                        ('Π', "Pi"),
                        ('Ρ', "Rho"),
                        ('Σ', "Sigma"),
                        ('Τ', "Tau"),
                        ('Υ', "Upsilon"),
                        ('Φ', "Phi"),
                        ('Χ', "Chi"),
                        ('Ψ', "Psi"),
                        ('Ω', "Omega")]

-- | Letter constructor.  Ensures that the combination of properties is valid
--   according to the rules of Greek orthography.
makeLetter :: Char -> Breathing -> Accent -> IotaSub -> Macron -> Letter
makeLetter base breathing accent iotaSub macron =
  if validLetter macron base breathing accent iotaSub
  then Letter base iotaSub breathing accent macron
  else error (intercalate " "
              ["makeLetter: invalid combination:",
               show base,
               show breathing,
               show accent,
               show iotaSub,
               show macron])

-- | 'Word' constructor.  We export this rather than the actual 'Word'
--   constructor to allow us the freedom of changing the 'Word' representation
--   later.
makeWord :: [Letter] -> Word
makeWord = Word

-- | Recognizes valid combinations of base letters and diacriticals
validLetter :: Macron -> Char -> Breathing -> Accent -> IotaSub -> Bool
validLetter macron base breathing accent iotaSub =
  validBase base
  && validMacron macron base accent iotaSub
  && validBreathing base breathing
  && validAccent base accent
  && validIotaSub base iotaSub

-- | Set of chars legal as base chars in a 'Letter'
baseChars :: Set Char
baseChars =
  Set.fromList "αβγδεϝζηθικλμνξοπρςστυφχψωΑΒΓΔΕϜΖΗΘΙΚΛΜΝΞΟΠΡΣΤΥΦΧΨΩ"

-- | Recognizes chars legal as base chars in a 'Letter'
validBase :: Char -> Bool
validBase c = c `Set.member` baseChars

-- | Recognizes valid uses of explicit macrons, given base char, accent, and
--   iota subscript.
validMacron :: Macron -> Char -> Accent -> IotaSub -> Bool
validMacron NoMacron _    _          _       = True
validMacron Macron   _    Circumflex _       = False
validMacron Macron   _    _          IotaSub = False
validMacron Macron   base _          _       = base `elem` "αιυΑΙΥ"

-- | Set of vowel base characters
vowels :: Set Char
vowels = Set.fromList "αεηιουωΑΕΗΙΟΥΩ"

-- | Recognizes valid use of breathing marks, given base char
validBreathing :: Char -> Breathing -> Bool
validBreathing c br =
  br == NoBreathing
  || c `Set.member` vowels
  || c == 'ρ'
  || c == 'Ρ'

-- | Recognizes valid uses of accents, given base char
validAccent :: Char -> Accent -> Bool
validAccent _ NoAccent = True
validAccent c Acute = c `Set.member` vowels
validAccent c Grave = c `Set.member` vowels
validAccent c Circumflex = c `elem` "αηιυωΑΗΙΥΩ"

-- | Recognizes valid uses of iota subscript, given base char
validIotaSub :: Char -> IotaSub -> Bool
validIotaSub _ NoIotaSub = True
validIotaSub c IotaSub   = c `elem` "αηωΑΗΩ"
