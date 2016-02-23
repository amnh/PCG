module BadReadGraph where

import Analysis.GenericFitch
import Bio.Phylogeny.Graph
import Control.Monad                (sequence_)
import Data.Functor                 ((<$))
import Data.Vector                  (singleton)
import File.Format.Fasta
import File.Format.Newick
import File.Format.Newick.Converter
import Text.Megaparsec

badReadGraph :: FilePath -> FilePath -> IO Graph
badReadGraph fastaPath newickPath = do
  fastaResult  <- parse (fastaStreamConverter DNA =<< fastaStreamParser)  fastaPath  <$> readFile fastaPath
  newickResult <- parse  newickStreamParser newickPath <$> readFile newickPath
  case (fastaResult, newickResult) of
    (Left  x, Left  y) -> mempty <$ sequence_ (putStrLn <$> [show x, show y])
    (Left  x, _      ) -> mempty <$ putStrLn (show x)
    (_      , Left  y) -> mempty <$ putStrLn (show y)
    (Right x, Right y) -> pure $ convertBothForest y [coerceFasta x]
  where
    coerceFasta = fmap (singleton . Just)
