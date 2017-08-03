module Main
  ( main
  , testSuite
  ) where


import qualified Text.Megaparsec.Custom.Test           as Megaparsec
import qualified File.Format.Fasta.Test                as Fasta
import qualified File.Format.Fastc.Test                as Fastc
import qualified File.Format.Newick.Test               as Newick
import qualified File.Format.TransitionCostMatrix.Test as TCM
import qualified File.Format.VertexEdgeRoot.Test       as VER
import           Test.Tasty


main :: IO ()
main = defaultMain testSuite


testSuite :: TestTree
testSuite = testGroup "Library Test Suite" 
    [ Megaparsec.testSuite
    , Fasta.testSuite
    , Fastc.testSuite
    , Newick.testSuite
    , TCM.testSuite
    , VER.testSuite
    ]

