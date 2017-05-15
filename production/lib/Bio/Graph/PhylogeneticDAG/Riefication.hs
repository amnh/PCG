------------------------------------------------------------------------------
-- |
-- Module      :  Bio.Graph.PhylogeneticDAG.Riefication
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

{-# LANGUAGE FlexibleContexts #-}

module Bio.Graph.PhylogeneticDAG.Riefication
  ( riefiedSolution
  , riefiedToCharacterDAG
  ) where

import           Bio.Graph
import           Bio.Graph.Node
import           Bio.Graph.PhylogeneticDAG.Internal
import           Bio.Graph.ReferenceDAG.Internal
import           Control.Lens
import           Control.Monad.State.Lazy
import           Data.Bits
import           Data.Foldable
import qualified Data.IntMap        as IM
import           Data.Key
import           Data.List.NonEmpty        (NonEmpty( (:|) ))
import qualified Data.List.NonEmpty as NE
import           Data.Maybe
import           Data.Semigroup
import           Data.Semigroup.Foldable
import qualified Data.Vector        as V
import           Prelude            hiding (zipWith)


-- |
-- Riefies a solution, performing several initialization functions on each DAG
-- before it's cost can be calculated.
riefiedSolution :: PhylogeneticSolution UnRiefiedCharacterDAG -> CharacterResult
riefiedSolution  = PhylogeneticSolution . fmap (fmap riefiedToCharacterDAG) . phylogeneticForests


-- |
-- Riefies a particular DAg so it has the requisite context for a post-order
-- traversal.
riefiedToCharacterDAG :: UnRiefiedCharacterDAG -> CharacterDAG
riefiedToCharacterDAG (PDAG dag) = PDAG2
    RefDAG
    { references = newRefs
    , rootRefs   = rootRefs  dag
    , graphData  = graphData dag
    }
  where
    dagSize   = length $ references dag

    newRefs = V.generate dagSize g
      where
        g i = IndexData <$> const (memo ! i) <*> parentRefs <*> childRefs $ references dag ! i

    memo = V.generate dagSize g
      where
        (buildLeafNodeAssignments, leafCount) = (`runState` 0) . traverse f $ references dag
          where
            f e
              | (not . null) (childRefs e) = pure Nothing
              | otherwise = do
                  c <- get
                  modify (+1)
                  pure $ Just c

        g i = newNode -- IndexData <$> const newNode <*> parentRefs <*> childRefs $ indexData
          where
            indexData = references dag ! i
            newNode =
                PNode2
                { resolutions          = res
                , nodeDecorationDatum2 = nodeDecorationDatum $ nodeDecoration indexData
                }
            res = pure
                ResInfo
                { totalSubtreeCost      = 0
                , localSequenceCost     = 0
                , subtreeEdgeSet        = mempty
                , leafSetRepresentation = bv
                , subtreeRepresentation = ns
                , characterSequence     = sequenceDecoration $ nodeDecoration indexData
                }
            
            (bv, ns) =
              case buildLeafNodeAssignments ! i of
                Just n  -> ( leafCount `singletonSubtreeLeafSet` n
                           , singletonNewickSerialization n
                           )
                Nothing ->
                  case IM.keys $ childRefs indexData of
                    x:xs -> ( foldr1 (.|.) $ leafSetRepresentation . NE.head . resolutions . (memo !) <$> (x:|xs)
                            , foldMap1 (subtreeRepresentation . NE.head . resolutions . (memo !)) (x:|xs)
                            )
                    []   -> error "Never occurs."

