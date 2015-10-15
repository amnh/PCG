module TestSuite.LibraryTests
  ( main
  , testSuite
  ) where

import Test.Tasty 
import qualified Text.Parsec.Custom.Test as CustomParsec
import qualified File.Format.Fasta.Test  as Fasta
import qualified File.Format.Fastc.Test  as Fastc
import qualified File.Format.Newick.Test as Newick

main :: IO ()
main = defaultMain testSuite

testSuite :: TestTree
testSuite = testGroup "Library Test Suite" 
  [ CustomParsec.testSuite
  , Fasta.testSuite
  , Fastc.testSuite
  , Newick.testSuite
  ]

