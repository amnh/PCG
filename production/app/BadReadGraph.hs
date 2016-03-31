module Main where

--import           Analysis.GenericFitch
--import Analysis.Parsimony.Binary.Optimization
import           Bio.Phylogeny.Graph
--import           Bio.Phylogeny.Graph.Utilities
import qualified Bio.Phylogeny.PhyloCharacter as Char
import           Bio.Phylogeny.Tree.Node
import           Bio.Sequence.Coded
import           Control.Monad                (liftM2)
import           Data.IntMap                  (IntMap)
import           Data.Vector                  (Vector, singleton)
import qualified Data.Vector        as V
import           File.Format.Fasta
import qualified File.Format.Newick as N
import           File.Format.Newick.Converter
import           PCG.Command.Types.Report.CharacterMatrix
import           PCG.Command.Types.Report.GraphViz
import           PCG.Command.Types.Report.Newick
import           PCG.Command.Types.Report.Metadata
--import           PCG.Command.Types.Report.TaxonMatrix
import           Text.Megaparsec

main :: IO ()
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

madRead :: IO DAG
madRead = badReadGraph "../../TestDat/fakeArtmor.fas" "../../TestDat/artmor.tre"
--badNodes = (V.filter (\n -> isLeaf n && null (encoded n))) <$> (nodes <$> madRead)
--badNames = (V.map (\n -> (IM.! (code n)) <$> (nodeNames <$> madRead))) <$> badNodes

madness :: IO DAG
madness = undefined --allOptimization 1 <$> madRead

outputMad :: IO ()
outputMad = outPutDot "TestArtmor.dot" =<< ((Graph . pure) <$> madRead) 

madNewick :: IO ()
madNewick = outPutNewick "TestArtmorNewick.new" =<< ((Graph . pure) <$> madness)

madMatrix :: IO ()
madMatrix = outPutMatrix "TestArtmorCharacterMat.csv" =<< ((Graph . pure) <$> madRead)

madMetadata :: IO ()
madMetadata = outPutMetadata "TestArtmorMetadata.csv" =<< ((Graph . pure) <$> madRead)

checkOuts :: IO (Vector Bool)
checkOuts = liftM2 (V.zipWith (\n e -> not (isLeaf n) && null (outNodes e))) (nodes <$> madRead) (edges <$> madRead)

bigShow :: IO (Vector String)
bigShow = undefined --showSeqs . allOptimization 1 <$> madRead

madNames :: IO (IntMap Identifier)
madNames = nodeNames <$> madRead

smallRead :: IO DAG
smallRead = badReadGraph "../../TestDat/ThreeNode.fas" "../../TestDat/ThreeNode.tre"

smallNum :: IO DAG
smallNum = undefined --allOptimization 1 <$> smallRead

showSeqs :: DAG -> Vector String
showSeqs inDag = fmap (\n -> show (code n) ++ ": " ++ show (flip unencodeMany ["A", "C", "G", "T", "-"] $ encoded n)) (nodes inDag) 

smallShow :: IO (Vector String)
smallShow = showSeqs <$> smallNum

fiveRead :: IO DAG
fiveRead = badReadGraph "../../TestDat/FiveNode.fas" "../../TestDat/FiveNode.tre"

fiveNum :: IO DAG
fiveNum = undefined --allOptimization 1 <$> fiveRead

fiveShow :: IO (Vector String)
fiveShow = showSeqs <$> fiveNum

singleMad :: IO Double
singleMad = undefined --rootCost . allOptimization 1 <$> badReadGraph "../../TestDat/fakeArtmor.fas" "../../TestDat/SingleArtmor.tre"

mediumTest :: IO DAG
mediumTest = undefined --allOptimization 1 <$> badReadGraph "../../TestDat/MediumCooked.fas" "../../TestDat/MediumCooked.tre"

checkNewick :: IO (Either ParseError N.NewickForest)
checkNewick = parse N.newickStreamParser "../../TestDat/MediumCooked.tre" <$> readFile "../../TestDat/MediumCooked.tre"


-- | More formal section to run a few tests
test :: IO ()
test = undefined --do
    --eric1 <- rootCost . allOptimization 1 <$> badReadGraph "../../TestDat/OptimizationTests/EricFasta1.fas" "../../TestDat/OptimizationTests/EricTree1.tre"
    --eric2 <- rootCost . allOptimization 1 <$> badReadGraph "../../TestDat/OptimizationTests/EricFasta1.fas" "../../TestDat/OptimizationTests/EricTree2.tre"
    --eric3 <- rootCost . allOptimization 1 <$> badReadGraph "../../TestDat/OptimizationTests/EricFasta1.fas" "../../TestDat/OptimizationTests/EricTree3.tre"
    --eric4 <- rootCost . allOptimization 1 <$> badReadGraph "../../TestDat/OptimizationTests/EricFasta1.fas" "../../TestDat/OptimizationTests/EricTree4.tre"
    --putStrLn("Test results: ")
    --putStrLn("  Eric 1, cost should be 267: " ++ show eric1)
    --putStrLn("  Eric 2, cost should be 266: " ++ show eric2)
    --putStrLn("  Eric 3, cost should be 267: " ++ show eric3)
    --putStrLn("  Eric 4, cost should be 267: " ++ show eric4)
    --artmor <- rootCost . allOptimization 1 <$> badReadGraph "../../TestDat/fakeArtmor.fas" "../../TestDat/SingleArtmor.tre"
    --putStrLn("  Fake Artmor, cost should be 5861: " ++ show artmor)
