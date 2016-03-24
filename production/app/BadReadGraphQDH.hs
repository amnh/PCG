module Main where

import Analysis.Parsimony.Binary.Optimization
import Analysis.GenericFitch
import Bio.Phylogeny.Graph
import Bio.Phylogeny.Graph.Utilities
import Bio.Phylogeny.Graph.Output
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

madRead = badReadGraph "../../TestDat/fakeArtmor.fas" "../../TestDat/artmor.tre"
--badNodes = (V.filter (\n -> isLeaf n && null (encoded n))) <$> (nodes <$> madRead)
--badNames = (V.map (\n -> (IM.! (code n)) <$> (nodeNames <$> madRead))) <$> badNodes
madness = rootCost . allOptimization 1 <$> madRead
outputMad = outPutDot "TestArtmor.dot" =<< ((Graph . pure) <$> madRead) 
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
