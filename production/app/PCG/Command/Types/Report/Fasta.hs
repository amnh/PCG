-----------------------------------------------------------------------------
-- |
-- Module      :  PCG.Command.Types.Report.Fasta
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Standard algorithm for implied alignment
-----------------------------------------------------------------------------

module PCG.Command.Types.Report.Fasta where

import Analysis.ImpliedAlignment.Internal
import Bio.PhyloGraph.Solution
import Bio.Sequence.Coded.Internal
import Data.Vector
import qualified Data.Vector as V

iaOutput :: AlignmentSolution DynamicChar -> Solution -> String
iaOutput align (Solution _ meta inForests) = concat $ zipWith (iaForest meta) align inForests
    where
        iaForest a f = concat $ zipWith iaSingle a f
        iaDAG oa d = concat $ zipWith iaNode (toList oa) (toList $ nodes d)

        iaNode :: (Int, Vector DynamicChar) -> NodeInfo -> String
        iaNode (pos, alignments) curNode = 
                where
                    nodeLine = "> " ++ (name curNode) ++ "\n"
                    textSeq = concat $ V.zipWith (\m c -> decodeOverAlphabet (alphabet m) c) meta alignments