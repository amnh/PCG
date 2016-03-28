module Main where

import           Analysis.Parsimony.Binary.Optimization
import           Bio.Phylogeny.Graph
import           Bio.Phylogeny.Graph.Utilities
import           File.Format.Fasta
import qualified File.Format.Newick as N
import           File.Format.Newick.Converter
import           PCG.Command.Types.Report.Metadata
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
    coerceFasta = fmap (pure . Just)

madRead :: IO DAG
madRead = badReadGraph "../../TestDat/fakeArtmor.fas" "../../TestDat/artmor.tre"

madness :: IO Double
madness = rootCost . allOptimization 1 <$> madRead
