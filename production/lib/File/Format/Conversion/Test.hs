module File.Format.Conversion.Test where


import Test.Custom
import Bio.Phylogeny.Graph.Data
import Bio.Phylogeny.Tree.Node
import Bio.Phylogeny.PhyloCharacter
import File.Format.Newick.Parser
import File.Format.Newick.Converter
import Text.Megaparsec
import Test.Tasty                 
import Test.Tasty.HUnit
import File.Format.Fasta.Parser

import qualified Data.IntMap as IM
import qualified Data.Vector as V
import qualified Data.HashMap.Lazy as HM
import qualified Data.IntSet as IS

newickParse = newickForestDefinition <* eof
defaultNode = Node 0 True True [] [] mempty mempty mempty mempty mempty mempty 0 0
makeEdge parents children cur nodes = EdgeSet (IS.fromList parents) (foldr (\c -> IM.insert c (makeEInfo cur c nodes)) mempty children)
makeEInfo myCode termCode nodes = EdgeInfo 0 (nodes V.! myCode) (nodes V.! termCode) Nothing

testSuite :: TestTree
testSuite = testGroup "Conversion functionality"
    [testGroup "Newick Converters" [simplestNewick]]

simplestNewick :: TestTree
simplestNewick = testGroup "Simplest Newick files" [threeNode]
    where
        --smallTrees = ["(1);", "((1,2),3);", "(((1,2),3),(4,5));"]
        threeNode = testCase "Two node tree converts" $ convertEquals newickParse "<(1,2);>" expectedThree convertGraph
        threeNames = IM.fromList [(0, "HTU 0"), (1, "2"), (2, "1")]
        threeSeqs = HM.fromList [("HTU 0", mempty), ("2", mempty), ("1", mempty)]
        threeNodes = V.fromList [defaultNode {isLeaf = False, children = [2,1]}, defaultNode {isRoot = False, parents = [0], code = 1}, defaultNode {isRoot = False, parents = [0], code = 2}]
        threeEdges = V.map (\n -> makeEdge (parents n) (children n) (code n) threeNodes) threeNodes
        expectedThree = Graph [DAG threeNames threeSeqs mempty threeNodes threeEdges 0]

        --fiveNode = testCase "Three node tree converts to a five node topology" $ convertEquals newickParse "<((1,2),3);>" expectedFive convertGraph
        --fiveNames = IM.fromList [(0, "HTU 0"), (1, "HTU 1"), (2, "1"), (3, "2"), (4, "3")]
        --fiveSeqs =  HM.insert "HTU 1" mempty $ HM.insert "3" mempty threeSeqs
        --fiveNodes = V.fromList [defaultNode {isLeaf = False, children = [2,1]}, defaultNode {isRoot = False, parents = [0], code = 1}, defaultNode {isRoot = False, parents = [0], code = 2}
        --                            , ]

        --twoSeqs = 