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

import qualified Test.Data.Greek.Normalize
import qualified Test.Data.Greek.Word
import qualified Test.Data.Greek.Output
import qualified Test.Data.Greek.Parser
import qualified Test.Data.Greek.Order

main :: IO ()
main =
  do c <- runTestTT tests
     when (errors c /= 0 || failures c /= 0)
       exitFailure

tests =
  "Greek" ~:
  [Test.Data.Greek.Normalize.tests,
   Test.Data.Greek.Word.tests,
   Test.Data.Greek.Output.tests,
   Test.Data.Greek.Parser.tests,
   Test.Data.Greek.Order.tests]
