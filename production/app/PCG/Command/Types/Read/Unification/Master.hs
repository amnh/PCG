-----------------------------------------------------------------------------
-- |
-- Module      :  PCG.Command.Types.Read.Unification.Master
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Containing the master command for unifying all input types: tree, metadata, and sequence
--
-----------------------------------------------------------------------------

module PCG.Command.Types.Read.Unification.Master where

import PCG.Command.Types.Read.Unification.UnificationError
import Bio.Phylogeny.Graph.Parsed
import Bio.Metadata.Class
import Bio.Sequence.Parsed
import Bio.Sequence.Parsed.Class
import Bio.Phylogeny.Graph
import Data.IntMap ((\\), elems)
import Data.Monoid
import Data.Vector (Vector)
import File.Format.TransitionCostMatrix

data FracturedParseResult
   = FPR
   { parsedChars  :: [TreeSeqs]
   , parsedMetas  :: [Vector CharInfo]
   , parsedTrees  :: Graph
   , relatedTcm   :: Maybe TCM
   , sourceFile   :: FilePath
   } deriving (Show)
          

-- | Takes in a list of parse results and outputs 
masterUnify :: (Metadata p, ParsedCharacters p, ParseGraph p) => [p] -> Either UnificationError Graph
masterUnify inResults = 
    let
        allMetadata = map unifyMetadata inResults
        allGraphs = map unifyGraph inResults
        allSeqs = map unifyCharacters inResults
    in Right mempty

-- | Verify that between two graphs, the taxa names are the same
checkTaxaMatch :: Graph -> Graph -> ([String], [String])
checkTaxaMatch (Graph g1) (Graph g2) = 
    let
        allNames1 = foldr1 (<>) (map nodeNames g1)
        allNames2 = foldr1 (<>) (map nodeNames g2)
    in (elems $ allNames1 \\ allNames2, elems $ allNames2 \\ allNames1)

-- | Verify that between a graph and parsed sequences, the taxa names match
checkTaxaSeqs :: Graph -> TreeSeqs -> ([String], [String])
checkTaxaSeqs = mempty
