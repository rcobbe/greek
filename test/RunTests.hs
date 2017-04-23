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

-- Pulls all of the Greek tests into one place that we can easily run in a
-- single operation.  To run within ghci:
--  > :load RunTests
--  > runTestTT tests
-- or just run 'main'.
--
-- To run from the shell:
--  $ runhaskell RunTests

module Main(main) where

import Control.Monad
import System.Exit
import Test.HUnit

import qualified NormalizeTest
import qualified WordTest
import qualified OutputTest
import qualified ParserTest
import qualified OrderTest

main :: IO ()
main =
  do c <- runTestTT tests
     when (errors c /= 0 || failures c /= 0)
       exitFailure

tests =
  "Greek" ~:
  [NormalizeTest.tests,
   WordTest.tests,
   OutputTest.tests,
   ParserTest.tests,
   OrderTest.tests]
