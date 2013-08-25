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
  concat,
  validLetter,
  baseChars,
  isUpper,
  isLower,
  toUpper,
  toLower
  )
where

import Prelude hiding (concat)
import qualified Data.Char as Char
import Data.Function (on)
import Data.List hiding (concat)
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

-- | Defines the ordering relation on 'Word's.  First, compare the base letters
--   alone, insensitive to case or other diacriticals.  Then use iota
--   subscripts as tie breakers, then breathing marks, then accents, then
--   macrons, and then finally case.
--
--   We have to write our own order relation on Greek words, because the
--   locale-aware Unicode routines don't do quite what we want.  In particular,
--   they order ᾳ between α and β, but Liddell and Scott treat it as a variant
--   of α, using the iota subscript as a tie breaker: ῥαδινός, ῥᾴδιος, ...,
--   ῥαθάμιγξ, ..., ῥᾳθυμέω, ..., ῥαιβόκρανος.  We follow Liddell & Scott.
instance Ord Word where
  compare w1 w2 =
    let xs = getLetters w1
        ys = getLetters w2
    in refineCompare [listCompare (baseCompare `on` getBase) xs ys,
                      listCompare (compare `on` getIotaSub) xs ys,
                      listCompare (compare `on` getBreathing) xs ys,
                      listCompare (compare `on` getAccent) xs ys,
                      listCompare (compare `on` getMacron) xs ys,
                      listCompare caseCompare xs ys]

-- | Compares two Greek letters (without diacriticals), ignoring case.  We
--   consider final sigma to be less than medial sigma.  If two words have the
--   same prefix, and the next character in the first word is final sigma, and
--   the next character in the second word is medial sigma, then the rules of
--   Greek orthography say that the first word must end after this character
--   and that the second word must not.  Treating final sigma as smaller means
--   that we consider the first word to be smaller, which is the desired
--   result.
baseCompare :: Char -> Char -> Ordering
baseCompare x y =
  let lowerX = Char.toLower x
      lowerY = Char.toLower y
  in if lowerX == lowerY
     then EQ
     else
       foldr (compareLetter lowerX lowerY)
        (error ("baseCompare '" ++ [x] ++ "' '" ++ [y] ++ "': " ++
                "fell off the end!"))
        "αβγδεϝζηθικλμνξοπρςστυφχψω"
  where compareLetter x y currLetter accum
          | x == currLetter && y == currLetter = EQ
          | x == currLetter                    = LT
          | y == currLetter                    = GT
          | otherwise                          = accum

-- | Compares two characters strictly on the basis of capitalization, ignoring
--   the underlying char.  Uppercase is less than lowercase.
caseCompare :: Letter -> Letter -> Ordering
caseCompare c1 c2 =
  compare (isLower c1) (isLower c2)

-- | Combines several comparison operations, with earlier ones taking
--   precedence of later ones.  Equivalently, returns first non-'EQ' value, or
--   'EQ' if none found.
refineCompare :: [Ordering] -> Ordering
refineCompare orderings =
  foldr refineOrderings EQ orderings
  where refineOrderings :: Ordering -> Ordering -> Ordering
        -- refineOrderings ordering accum
        refineOrderings EQ accum = accum
        refineOrderings ordering _ = ordering

-- | Compares two lists lexicographically, using the supplied function to
--   compare elements.  If either list is a prefix of the other, then the
--   prefix is less; otherwise, the difference between the first set of
--   non-'EQ' elements encountered determines the answer.
listCompare :: (a -> a -> Ordering) -> [a] -> [a] -> Ordering
listCompare compare [] [] = EQ
listCompare compare [] _ = LT
listCompare compare _ [] = GT
listCompare compare (x:xs) (y:ys) =
  case compare x y of
    LT -> LT
    EQ -> listCompare compare xs ys
    GT -> GT

-- | Letter constructor.  Ensures that the combination of properties is valid
--   according to the rules of Greek orthography.  If the combination is
--   invalid, calls 'error'.  For a more robust way to deal with user input,
--   use the functions in "Data.Greek.Parser".
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
makeWord letters =
  if null letters
  then error "makeWord: argument cannot be empty"
  else Word letters

-- | Concatenates two 'Word' values.
concat :: Word -> Word -> Word
concat (Word xs) (Word ys) = Word (xs ++ ys)

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

-- | Selects upper-case 'Letter's.
isUpper :: Letter -> Bool
isUpper c = Char.isUpper (base c)

-- | Selects lower-case 'Letter's.
isLower :: Letter -> Bool
isLower c = Char.isLower (base c)

-- | Converts a 'Letter' to the corresponding uppercase 'Letter', preserving
--   all diacriticals.  Leaves uppercase 'Letter's unchanged.
toUpper :: Letter -> Letter
toUpper l = l { base = Char.toUpper (base l) }

-- | Converts a 'Letter' to the corresponding lowercase 'Letter', preserving
--   all diacriticals.  Leaves lowercase 'Letter's unchanged.  Converts capital
--   sigma to lowercase /medial/ sigma.
toLower :: Letter -> Letter
toLower l = l { base = Char.toLower (base l) }
