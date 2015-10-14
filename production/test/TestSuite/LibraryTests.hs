module TestSuite.LibraryTests
  ( main
  , testSuite
  ) where

import Test.Tasty 
import qualified Text.Parsec.Custom.Test as CustomParsec
import qualified File.Format.Fasta.Test  as Fasta

main :: IO ()
main = defaultMain testSuite

testSuite :: TestTree
testSuite = testGroup "Library Test Suite" [ CustomParsec.testSuite, Fasta.testSuite ]

