-----------------------------------------------------------------------------
-- |
-- Module      :  PCG.Command.Types.Report.Metadata
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Functionality to output a csv containing all the metadata
--
-----------------------------------------------------------------------------

module PCG.Command.Types.Report.Metadata where

import Bio.Phylogeny.Solution
import Bio.Metadata
import Data.Foldable
import Data.List   (intercalate)
import Data.Monoid ((<>))
import Data.Vector (Vector)

--import Debug.Trace

-- TODO: use spreadsheet library for tabular output files
-- | Wrapper function to output a metadata csv
outPutMetadata :: FilePath -> StandardSolution -> IO ()
outPutMetadata fileName = writeFile fileName . metadataCsvOutput

metadataCsvOutput :: StandardSolution -> String
metadataCsvOutput solution = header <> mainExport (metadata solution)
    where
        header = "Type, Name, Aligned, Additive, State Names, Alphabet, Ignored, Weight \n"
        mainExport :: Vector StandardMetadata -> String
        mainExport = intercalate "\n" . fmap fetchInfo . toList 

--metadataCsvOutput :: Graph -> String
--metadataCsvOutput (Graph dags) = ifoldr oneCSV header (fromList dags)
--    where
--        header = "DAG, Type, Name, Aligned, Additive, State Names, Alphabet, Ignored, Weight \n"

--        -- | Main creation functionality
--        oneCSV :: Int -> DAG -> String -> String
----        oneCSV _index inDAG _curStr | trace ("oneCSV " ++ show (characters inDAG)) False = undefined
--        oneCSV index inDAG curStr = foldl (\acc c -> acc ++ show index ++ ", " ++ fetchInfo c ++ "\n") curStr myMeta
--            where
--                myMeta = characters inDAG

fetchInfo :: Show s => CharacterMetadata s -> String
fetchInfo c = intercalate ", " [show $ charType c, name c, show $ isAligned c, show $ isAdditive c, show $ stateNames c, show $ alphabet c, show $ isIgnored c, show $ weight c]

foldInfo :: (Foldable t, Show a) => t a -> String  
foldInfo = unwords . fmap show . toList

