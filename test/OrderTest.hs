{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

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

-- | QuickCheck tests for ordering of 'Greek.Word'.
--
--   For many of the ordering properties, we use custom generators that
--   generate one word and then generate the second by perturbing specific
--   properties of the first.  In an earlier attempt, we used generators that
--   synthesized two words independently of each other, and the relevant
--   properties like 'propAccents' stipulated (with '==>') that the words were
--   identical in all but the interesting aspecs.  This works, in principle,
--   but it turns out to be so unlikely that the generator produces words that
--   satisfy this constraint that the tests were failing due to lack of
--   acceptable data.
module OrderTest(tests) where

import Prelude hiding (Word)
import qualified Data.Char as Char
import Data.Map ((!), Map)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Test.HUnit ((~:))
import qualified Test.HUnit as HU
import Test.QuickCheck ((==>), (.&&.), (.||.))
import qualified Test.QuickCheck as QC

import Data.Greek.Word
import Data.Greek.UnicodeData

-- temporary
-- import Greek.Parser

import GreekLetterGen

import Test.Utils

-- to run a test:
--   QC.quickCheck prop
--   QC.quickCheckWith (QC.stdArgs { QC.maxSuccess = 1000 }) prop

qc1000 :: (QC.Testable a) => a -> HU.Test
qc1000 = testQuickCheck (QC.stdArgs { QC.maxSuccess = 1000 })

tests =
  "Test.Data.Greek.Order" ~: [
    "ordering respects equality" ~: qc1000 propOrderRespectsEquality,

    "ordering is reflexive" ~: qc1000 propReflexive,

    "ordering is antisymmetric" ~: qc1000 propAntisymmetric,

    -- skip transitivity; we can't generate enough valid input

    "ordering is total" ~: qc1000 propTotalOrder,

    "prefix always less" ~: qc1000 propPrefixLess,

    "base character difference is most significant" ~:
    qc1000 propBase,

    "then iota subscript" ~:
    qc1000 propIotaSub,

    "breathing difference" ~:
    qc1000 propBreathing,

    "accent difference" ~:
    qc1000 propAccents,

    "macron difference" ~:
    qc1000 propMacrons,

    "case difference is least significant" ~:
    qc1000 propCase
    ]

-- | The order relation is consistent with the (implied) equality relation.
propOrderRespectsEquality :: Word -> Word -> Bool
propOrderRespectsEquality w1 w2 = (w1 == w2) == (compare w1 w2 == EQ)

-- | The order relation is reflexive.
propReflexive :: Word -> Bool
propReflexive w = w <= w

-- | The order relation is antisymmetric.
propAntisymmetric :: Word -> Word -> Bool
propAntisymmetric w1 w2 = ((w1 <= w2) && (w2 <= w1)) == (w1 == w2)

-- | The order relation is transitive.  (We don't actually test this one at the
--   moment, since it's too hard to come up with interesting input.)
propTransitive :: Word -> Word -> Word -> QC.Property
propTransitive w1 w2 w3 = (w1 <= w2) && (w2 <= w3) ==> (w1 <= w3)

-- | The order relation is total: every pair of elements is comparable.
propTotalOrder :: Word -> Word -> Bool
propTotalOrder w1 w2 = (w1 < w2) || (w1 == w2) || (w1 > w2)

-- | If word w1 is a prefix of w2, then w1 <= w2.
propPrefixLess :: PrefixWords -> Bool
propPrefixLess (PrefixWords w1 w2) = w1 <= w2

-- | If two words have base characters that differ in more than case, then that
--   difference determines the result of comparing the words.
propBase w1 w2 =
  let base1 = getLowerBase w1
      base2 = getLowerBase w2
  in
   (base1 /= base2) ==> (compareListBy compareBase base1 base2 == compare w1 w2)

-- | If two words differ only in iota subscripts, case, and other diacriticals,
--   then the iota subscript difference determines the overall difference.
propIotaSub (IotaSubWords w1 w2) =
  makeOrderProperty getIotaSub w1 w2

-- | If two words differ only in breathing, accent, macrons, and case, then
--   the breathing difference determines the overall difference.
propBreathing (BreathingWords w1 w2) =
  makeOrderProperty getBreathing w1 w2

-- | If two words differ only in accent, macrons, and case, then the accent
--   difference determines the overall difference.
propAccents (AccentWords w1 w2) =
  makeOrderProperty getAccent w1 w2

-- | If two words differ only in macrons and case, then the difference in
--   macrons is the difference between the words
propMacrons (MacronWords w1 w2) =
  makeOrderProperty getMacron w1 w2

-- | If two words differ only in case, then that difference determines the
--   result of comparing the words.
propCase (CaseWords w1 w2) =
  makeOrderProperty (Char.isLower . getBase) w1 w2

-- | Constructs an order property on 'Word' objects.  @makeOrderProperty
--   extractProp@ creates a property parameterized over two words, @w1@ and
--   @w2@, that says that, if @w1@ and @w2@ differ in the property selected
--   by @extractProp@, then the result of comparing the words is the same as
--   the result of comparing the property by itself.  This function assumes
--   that the words agree on all properties more significant to ordering than
--   the one selected by @extractProp@.
makeOrderProperty :: Ord a => (Letter -> a) -> Word -> Word -> QC.Property
makeOrderProperty extractProp w1 w2 =
  let props1 = map extractProp (getLetters w1)
      props2 = map extractProp (getLetters w2)
  in
   (props1 /= props2) ==> (compareList props1 props2 == compare w1 w2)

-- | Lexicographic ordering on lists, using the supplied function to compare
--   list elements.
compareListBy :: (a -> a -> Ordering) -> [a] -> [a] -> Ordering
compareListBy _ [] [] = EQ
compareListBy _ [] _  = LT
compareListBy _ _  [] = GT
compareListBy f (x:xs) (y:ys) =
  case (f x y) of
    EQ -> compareListBy f xs ys
    other -> other

-- | Lexicographic ordering on lists, using 'compare' on elements.
compareList :: (Ord a) => [a] -> [a] -> Ordering
compareList xs ys = compareListBy compare xs ys

-- | Compares two lowercase Greek base characters (i.e., no precomposed
--   diacriticals)
compareBase :: Char -> Char -> Ordering
compareBase c1 c2 = compare (baseOrdering ! c1) (baseOrdering ! c2)

-- | Defines an ordering on lowercase base characters by mapping them to
--   ordinals.
baseOrdering :: Map Char Int
baseOrdering =
  -- We carefully put final sigma before medial sigma to get the desired
  -- ordering.
  Map.fromList (zip "αβγδεϝζηθικλμνξοπρςστυφχψω" [1..])

-- | Returns base letters, with no diacriticals, downcased.
getLowerBase :: Word -> [Char]
getLowerBase w = map Char.toLower (map getBase (getLetters w))
