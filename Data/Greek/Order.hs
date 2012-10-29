module Data.Greek.Order() where

import Data.Char (toLower, isLower)
import Data.Function (on)

import Data.Greek.Word

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
                      listCompare (caseCompare `on` getBase) xs ys]

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
  let lowerX = toLower x
      lowerY = toLower y
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
caseCompare :: Char -> Char -> Ordering
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
