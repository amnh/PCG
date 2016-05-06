-----------------------------------------------------------------------------
-- |
-- Module      :  PCG.Command.Types.Report.ImpliedalignmentFasta
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Standard algorithm for implied alignment
-----------------------------------------------------------------------------

module PCG.Command.Types.Report.ImpliedAlignmentFasta where

import           Analysis.ImpliedAlignment.Internal
import           Bio.Character.Dynamic.Coded
import           Bio.Metadata
import           Bio.PhyloGraph.DAG
import           Bio.PhyloGraph.Solution
import           Data.Foldable
import           Data.Vector   (Vector)
import qualified Data.Vector as V

iaOutput :: AlignmentSolution DynamicChar -> Solution a -> [(FilePath, String)]
iaOutput align (Solution _ meta inForests) = undefined{- concat $ zipWith (iaForest meta) align inForests
  where
    iaForest a f = concat $ zipWith iaDAG a f
    iaDAG   oa d = concat $ zipWith iaNode (toList oa) (toList $ getNodes d)

    iaNode :: (Int, Vector DynamicChar) -> NodeInfo -> [String]
    iaNode (pos, alignments) curNode = fmap (\t -> nodeLine ++ t) textSeq
      where
        nodeLine = "> " ++ (name curNode) ++ "\n"
        textSeq = V.zipWith (\m c -> decodeOverAlphabet (alphabet m) c) meta alignments -}
