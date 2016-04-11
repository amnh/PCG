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
        header = "Type, Name, Aligned, State Names, Alphabet, Ignored, Weight \n"
        mainExport :: Vector StandardMetadata -> String
        mainExport = intercalate "\n" . fmap fetchInfo . toList 

fetchInfo :: Show s => CharacterMetadata s -> String
fetchInfo c = intercalate ", " [show $ charType c, name c, show $ isAligned c, show $ stateNames c, show $ alphabet c, show $ isIgnored c, show $ weight c]

foldInfo :: (Foldable t, Show a) => t a -> String  
foldInfo = unwords . fmap show . toList

