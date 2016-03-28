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

import Prelude hiding (foldr)
import Data.Vector (fromList, ifoldr)
import Data.Foldable (foldr)

import Bio.Phylogeny.Graph
import Bio.Phylogeny.PhyloCharacter

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
        oneCSV index inDAG curStr = foldr (\c acc -> acc ++ show index ++ ", " ++ fetchInfo c ++ "\n") curStr myMeta
            where
                myMeta = characters inDAG

                fetchInfo :: Show s => PhyloCharacter s -> String
                fetchInfo inChar = case inChar of
                    (DNA n a _ s alph _ i w)            -> "DNA" ++ ", " ++ n ++ ", " ++ show a ++ ",, " ++ foldInfo s ++ ", " ++ foldInfo alph ++ ", " ++ show i ++ ", " ++ show w
                    (RNA n a _ s alph _ i w)            -> "RNA" ++ ", " ++ n ++ ", " ++ show a ++ ",, " ++ foldInfo s ++ ", " ++ foldInfo alph ++ ", " ++ show i ++ ", " ++ show w
                    (Qualitative n _ a s alph _ add i w)-> "Qualitative" ++ ", " ++ n ++ ", " ++ show a ++ ", " ++ show add ++ ", " ++ foldInfo s ++ ", " ++ foldInfo alph ++ ", " ++ show i ++ ", " ++ show w
                    (Continous n i _ alph w)            -> "Continuous" ++ ", " ++ n ++ ",,,, " ++ foldInfo alph ++ ", " ++ show i ++ ", " ++ show w
                    (Custom n a _ alph s _ i add w)     -> "Custom" ++ ", " ++ n ++ ", " ++ show a ++ ", " ++ show add ++ ", " ++ foldInfo s ++ ", " ++ foldInfo alph ++ ", " ++ show i ++ ", " ++ show w
                    (AminoAcid n a _ alph s _ i w)      -> "Amino Acid" ++ ", " ++ n ++ ", " ++ show a ++ ",, " ++ foldInfo s ++ ", " ++ foldInfo alph ++ ", " ++ show i ++ ", " ++ show w

                foldInfo :: (Foldable t, Show a) => t a -> String  
                foldInfo = foldl (\acc val -> acc ++ " " ++ show val) mempty 

