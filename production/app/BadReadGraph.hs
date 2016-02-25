module BadReadGraph where

import Analysis.GenericFitch
import Bio.Phylogeny.Graph
import Bio.Phylogeny.Graph.Utilities
import Control.Monad                (sequence_)
import Data.Functor                 ((<$))
import Data.Vector                  (singleton)
import File.Format.Fasta
import File.Format.Newick
import File.Format.Newick.Converter
import Text.Megaparsec

badReadGraph :: FilePath -> FilePath -> IO Tree
badReadGraph fastaPath newickPath = do
  fastaResult  <- parse (fastaStreamConverter DNA =<< fastaStreamParser)  fastaPath  <$> readFile fastaPath
  newickResult <- parse  newickStreamParser newickPath <$> readFile newickPath
  case (fastaResult, newickResult) of
    (Left  x, Left  y) -> mempty <$ sequence_ (putStrLn <$> [show x, show y])
    (Left  x, _      ) -> mempty <$ putStrLn (show x)
    (_      , Left  y) -> mempty <$ putStrLn (show y)
    (Right x, Right y) -> pure $ convertBoth (head y) (coerceFasta x)
  where
    coerceFasta = fmap (singleton . Just)

madness = rootCost <$> allOptimization 1 <$> badReadGraph "../../TestDat/fakeArtmor.fas" "../../TestDat/artmor.tre"
