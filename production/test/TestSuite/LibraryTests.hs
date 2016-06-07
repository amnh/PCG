module TestSuite.LibraryTests
  ( main
  , testSuite
  ) where

import qualified Analysis.ImpliedAlignment.Test        as IA
import qualified Analysis.General.Test                 as GeneralAnalysis
import qualified Analysis.Parsimony.Binary.Test        as Parsimony
import qualified Bio.Character.Dynamic.Coded.Test      as Char
import qualified Bio.PhyloGraph.DAG.Test               as DAG
import qualified Test.Custom.Tree.Test                 as MockTree
{-
import qualified Control.Evaluation.Test               as Evaluation
import qualified Text.Megaparsec.Custom.Test           as Megaparsec
import qualified File.Format.Fasta.Test                as Fasta
import qualified File.Format.Fastc.Test                as Fastc
import qualified File.Format.Newick.Test               as Newick
import qualified File.Format.TransitionCostMatrix.Test as TCM
import qualified File.Format.VertexEdgeRoot.Test       as VER
-}
import           Test.Tasty

main :: IO ()
main = defaultMain testSuite

testSuite :: TestTree
testSuite = testGroup "Library Test Suite" 
  [

{-
  , Evaluation.testSuite
  , Megaparsec.testSuite
  , Fasta.testSuite
  , Fastc.testSuite
  , Newick.testSuite
  , TCM.testSuite
  , VER.testSuite
-}
    DAG.testSuite
  , MockTree.testSuite
  , Char.testSuite
  , GeneralAnalysis.testSuite
  , Parsimony.testSuite
  , IA.testSuite
  ]

