-----------------------------------------------------------------------------
-- |
-- Module      :  PCG.Command.Types.Report.DynamicCharacterTable
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts #-}

module PCG.Command.Types.Report.DynamicCharacterTable where

import Bio.Character.Encodable
import Bio.Character.Decoration.Dynamic
import Bio.Sequence
import Bio.PhyloGraphPrime
import Bio.PhyloGraphPrime.PhylogeneticDAG
import Bio.PhyloGraphPrime.Node
import Bio.PhyloGraphPrime.ReferenceDAG
import Control.Lens
import Data.Foldable
import Data.Monoid            ((<>))
import Data.List              (intercalate)
import qualified Data.List.NonEmpty as NE


-- |
-- Outputs tabluar data of the first dynamic character of the first network in
-- the first forest of the solution.
outputDynamicCharacterTablularData 
  :: DirectOptimizationDecoration z a
  => PhylogeneticSolution (PhylogeneticDAG e n u v w x y z)
  -> String
outputDynamicCharacterTablularData = generateTabularData . head . toList . NE.head . phylogeneticForests


generateTabularData
  :: DirectOptimizationDecoration z a
  => PhylogeneticDAG e n u v w x y z
  -> String
generateTabularData (PDAG dag) = header <> nodeFoldMap f dag
  where
    header = intercalate "," [ "Local Cost", "Subtree Cost", "Preliminary Medians", "Final Medians" ] <> "\n"
    f node =
      case take 1 . foldMap (toList . dynamicCharacters) . toBlocks $ sequenceDecoration node of
        []  -> "No Dynamic Characters!\n"
        x:_ -> let alphabet           = x ^. characterAlphabet  
                   localCost          = show $ x ^. characterLocalCost
                   subTreeCost        = show $ x ^. characterCost
                   preliminaryMedians = showStream alphabet $ x ^. preliminaryGapped
                   finalMedians       = showStream alphabet $ x ^.       finalGapped
               in  intercalate "," [localCost, subTreeCost, preliminaryMedians, finalMedians] <> "\n"
