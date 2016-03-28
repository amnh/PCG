module Main where

--import Analysis.GenericFitch
import Analysis.Parsimony.Binary.Optimization
import Bio.Phylogeny.Graph
import Bio.Phylogeny.Graph.Utilities
import PCG.Command.Types.Report.CharacterMatrix
import PCG.Command.Types.Report.GraphViz
import PCG.Command.Types.Report.Newick
import PCG.Command.Types.Report.Metadata
import Bio.Phylogeny.Tree.Node
import Control.Monad                (sequence_, liftM2)
import Data.Functor                 ((<$))
import Data.Vector                  (singleton)
import qualified Data.IntMap as IM
import qualified Data.Vector as V
import File.Format.Fasta
import qualified File.Format.Newick as N
import File.Format.Newick.Converter
import Text.Megaparsec
import Bio.Sequence.Coded
import qualified Bio.Phylogeny.PhyloCharacter as Char

main = print =<< madness

badReadGraph :: FilePath -> FilePath -> IO DAG
badReadGraph fastaPath newickPath = do
  fastaResult  <- parse (fastaStreamConverter DNA =<< fastaStreamParser)  fastaPath  <$> readFile fastaPath
  newickResult <- parse N.newickStreamParser newickPath <$> readFile newickPath
  case (fastaResult, newickResult) of
    (Left  x, Left  y) -> mempty <$ sequence_ (putStrLn <$> [show x, show y])
    (Left  x, _      ) -> mempty <$ print x
    (_      , Left  y) -> mempty <$ print y
    (Right x, Right y) -> pure $ convertBoth (head y) (coerceFasta x)
  where
    coerceFasta = fmap (singleton . Just)

forceUnaligned :: DAG -> DAG
forceUnaligned inDAG = inDAG {characters = V.map (\c -> c {Char.aligned = False}) (characters inDAG)}

madRead = badReadGraph "../../TestDat/fakeArtmor.fas" "../../TestDat/artmor.tre"
--badNodes = (V.filter (\n -> isLeaf n && null (encoded n))) <$> (nodes <$> madRead)
--badNames = (V.map (\n -> (IM.! (code n)) <$> (nodeNames <$> madRead))) <$> badNodes
madness = allOptimization 1 <$> madRead
outputMad = outPutDot "TestArtmor.dot" =<< ((Graph . pure) <$> madRead) 
madNewick = outPutNewick "TestArtmorNewick.new" =<< ((Graph . pure) <$> madness)
madMatrix = outPutMatrix "TestArtmorCharacterMat.csv" =<< ((Graph . pure) <$> madRead)
madMetadata = outPutMetadata "TestArtmorMetadata.csv" =<< ((Graph . pure) <$> madRead)
checkOuts = liftM2 (V.zipWith (\n e -> not (isLeaf n) && null (outNodes e))) (nodes <$> madRead) (edges <$> madRead)
bigShow = showSeqs . allOptimization 1 <$> madRead
madNames = nodeNames <$> madRead

smallRead = badReadGraph "../../TestDat/ThreeNode.fas" "../../TestDat/ThreeNode.tre"
smallNum = allOptimization 1 <$> smallRead
showSeqs inDag = fmap (\n -> show (code n) ++ ": " ++ show (flip unencodeMany ["A", "C", "G", "T", "-"] $ encoded n)) (nodes inDag) 
smallShow = showSeqs <$> smallNum

fiveRead = badReadGraph "../../TestDat/FiveNode.fas" "../../TestDat/FiveNode.tre"
fiveNum = allOptimization 1 <$> fiveRead
fiveShow = showSeqs <$> fiveNum

singleMad = rootCost . allOptimization 1 <$> badReadGraph "../../TestDat/fakeArtmor.fas" "../../TestDat/SingleArtmor.tre"

mediumTest = allOptimization 1 <$> badReadGraph "../../TestDat/MediumCooked.fas" "../../TestDat/MediumCooked.tre"

checkNewick = parse N.newickStreamParser "../../TestDat/MediumCooked.tre" <$> readFile "../../TestDat/MediumCooked.tre"


-- | More formal section to run a few tests
test :: IO ()
test = do
    eric1 <- rootCost . allOptimization 1 <$> badReadGraph "../../TestDat/OptimizationTests/EricFasta1.fas" "../../TestDat/OptimizationTests/EricTree1.tre"
    eric2 <- rootCost . allOptimization 1 <$> badReadGraph "../../TestDat/OptimizationTests/EricFasta1.fas" "../../TestDat/OptimizationTests/EricTree2.tre"
    eric3 <- rootCost . allOptimization 1 <$> badReadGraph "../../TestDat/OptimizationTests/EricFasta1.fas" "../../TestDat/OptimizationTests/EricTree3.tre"
    eric4 <- rootCost . allOptimization 1 <$> badReadGraph "../../TestDat/OptimizationTests/EricFasta1.fas" "../../TestDat/OptimizationTests/EricTree4.tre"
    putStrLn("Test results: ")
    putStrLn("  Eric 1, cost should be 267: " ++ show eric1)
    putStrLn("  Eric 2, cost should be 266: " ++ show eric2)
    putStrLn("  Eric 3, cost should be 267: " ++ show eric3)
    putStrLn("  Eric 4, cost should be 267: " ++ show eric4)
    artmor <- rootCost . allOptimization 1 <$> badReadGraph "../../TestDat/fakeArtmor.fas" "../../TestDat/SingleArtmor.tre"
    putStrLn("  Fake Artmor, cost should be 5861: " ++ show artmor)
