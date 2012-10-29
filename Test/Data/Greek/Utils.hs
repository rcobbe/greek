module Test.Data.Greek.Utils(assertNormalizedParse)
where

import Data.Text.Lazy (Text)
import Test.HUnit
import Text.Parsec.Text.Lazy

import Data.Greek.Normalize
import Test.Utils

assertNormalizedParse :: (Eq a, Show a)
                         => a
                         -> GenParser () a
                         -> Text
                         -> Assertion
assertNormalizedParse expected parser input =
  assertParse expected parser (normalize input)
