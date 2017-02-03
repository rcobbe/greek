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
