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

import Bio.Phylogeny.Graph
import Bio.Phylogeny.PhyloCharacter
import Data.Foldable
import Data.List   (intercalate)
import Data.Vector (fromList, ifoldr)

--import Debug.Trace

-- TODO: use spreadsheet library for tabular output files
-- | Wrapper function to output a metadata csv
outPutMetadata :: FilePath -> Graph -> IO ()
outPutMetadata fileName = writeFile fileName . metadataCsvOutput

metadataCsvOutput :: Graph -> String
metadataCsvOutput (Graph dags) = ifoldr oneCSV header (fromList dags)
    where
        header = "DAG, Type, Name, Aligned, Additive, State Names, Alphabet, Ignored, Weight \n"

        -- | Main creation functionality
        oneCSV :: Int -> DAG -> String -> String
--        oneCSV _index inDAG _curStr | trace ("oneCSV " ++ show (characters inDAG)) False = undefined
        oneCSV index inDAG curStr = foldl (\acc c -> acc ++ show index ++ ", " ++ fetchInfo c ++ "\n") curStr myMeta
            where
                myMeta = characters inDAG

fetchInfo :: Show s => PhyloCharacter s -> String
fetchInfo c = intercalate ", " $
  case c of
    DNA         {} -> ["DNA"        , name c, show $ aligned c,                "", foldInfo $ stateNames c, foldInfo $ alphabet c, show $ ignored c, show $ weight c ]
    RNA         {} -> ["RNA"        , name c, show $ aligned c,                "", foldInfo $ stateNames c, foldInfo $ alphabet c, show $ ignored c, show $ weight c ]
    Qualitative {} -> ["Qualitative", name c, show $ aligned c, show $ additive c, foldInfo $ stateNames c, foldInfo $ alphabet c, show $ ignored c, show $ weight c ]
    Continous   {} -> ["Continuous" , name c,               "",                "",                      "", foldInfo $ alphabet c, show $ ignored c, show $ weight c ]
    Custom      {} -> ["Custom"     , name c, show $ aligned c, show $ additive c, foldInfo $ stateNames c, foldInfo $ alphabet c, show $ ignored c, show $ weight c ]
    AminoAcid   {} -> ["Amino Acid" , name c, show $ aligned c,                "", foldInfo $ stateNames c, foldInfo $ alphabet c, show $ ignored c, show $ weight c ]

foldInfo :: (Foldable t, Show a) => t a -> String  
foldInfo = unwords . fmap show . toList

