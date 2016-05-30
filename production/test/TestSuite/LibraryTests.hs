module TestSuite.LibraryTests
  ( main
  , testSuite
  ) where

import qualified Analysis.ImpliedAlignment.Test        as IA
import qualified Analysis.General.Test                 as GeneralAnalysis
import qualified Analysis.Parsimony.Binary.Test        as Parsimony
import qualified Bio.Character.Dynamic.Coded.Test      as Char
import qualified Bio.PhyloGraph.DAG.Test               as DAG
import qualified Control.Evaluation.Test               as Evaluation
import qualified Text.Megaparsec.Custom.Test           as Megaparsec
import qualified File.Format.Fasta.Test                as Fasta
import qualified File.Format.Fastc.Test                as Fastc
import qualified File.Format.Newick.Test               as Newick
import qualified File.Format.TransitionCostMatrix.Test as TCM
import qualified File.Format.VertexEdgeRoot.Test       as VER
import qualified Test.Custom.Tree.Test                 as TestTree
import           Test.Tasty

main :: IO ()
main = defaultMain testSuite

testSuite :: TestTree
testSuite = testGroup "Library Test Suite" 
  [ Evaluation.testSuite
  , TestTree.testSuite
  , Megaparsec.testSuite
  , DAG.testSuite
  , Fasta.testSuite
  , Fastc.testSuite
  , Newick.testSuite
  , TCM.testSuite
  , VER.testSuite
  , Char.testSuite
  , GeneralAnalysis.testSuite
  , Parsimony.testSuite
  , IA.testSuite
  ]

