------------------------------------------------------------------------------
-- |
-- Module      :  Bio.Graph.PhylogeneticDAG.Reification
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

module Bio.Graph.PhylogeneticDAG.Reification
  ( reifiedSolution
  ) where

import           Bio.Graph.Constructions
import           Bio.Graph.Node
import           Bio.Graph.PhylogeneticDAG.Internal (setDefaultMetadata)
import           Bio.Graph.ReferenceDAG.Internal
import           Bio.Graph.Solution
import           Control.Applicative
import           Control.Lens
import           Control.Monad.State.Lazy
import           Data.Bits
import           Data.Foldable
import           Data.Functor                       (($>))
import           Data.IntMap                        (IntMap)
import qualified Data.IntMap                        as IM
import           Data.IntSet                        (IntSet)
import           Data.Key
import           Data.List.NonEmpty                 (NonEmpty ((:|)))
import qualified Data.List.NonEmpty                 as NE
import           Data.Maybe
import           Data.Semigroup.Foldable
import           Data.Vector                        (Vector)
import qualified Data.Vector                        as V
import           Prelude                            hiding (zipWith)

-- import Debug.Trace


-- |
-- Reifies a particular DAG so it has the requisite context for a post-order
-- traversal.
--reifiedToCharacterDAG :: UnReifiedCharacterDAG -> CharacterDAG
--reifiedToCharacterDAG (PDAG dag) = PDAG2


reifiedSolution :: PhylogeneticSolution UnReifiedCharacterDAG -> CharacterResult
reifiedSolution = PhylogeneticSolution . fmap reifyForest . phylogeneticForests


-- |
-- Reifies a Forest so it has the requisite context for a post-order traversal.
-- Specifically, each leaf in the forest has a unique bitvector index and subtree
-- representation symbol.
reifyForest :: PhylogeneticForest UnReifiedCharacterDAG -> PhylogeneticForest CharacterDAG
reifyForest forest = zipWith (reifyDAGWithContext leavesInForest) leafMaskForest forest
  where
    (leafMaskForest, leavesInForest) = tabulateLeaves forest


tabulateLeaves :: PhylogeneticForest UnReifiedCharacterDAG -> (PhylogeneticForest (ReferenceDAG () () (Maybe Int)), Int)
tabulateLeaves = {- (\v@(x,_) -> trace ("Tab Vector:\n\n"  <> foldMap1 (\y -> show $ toList y) x) v) . -}
                 (`runState` 0) . traverse1 tabulateDAG
  where
    tabulateDAG :: UnReifiedCharacterDAG -> State Int (ReferenceDAG () () (Maybe Int))
    tabulateDAG (PDAG _ dag) = liftA3 ReferenceDAG newRefs rootRefsContext graphDataContext
      where
        rootRefsContext  = pure $ rootRefs dag
        graphDataContext = pure . defaultGraphMetadata $ graphData dag

        getLeafIndex :: Int -> State Int (Maybe Int)
        getLeafIndex i
          | notLeafRef i = pure Nothing
          | otherwise    = do
              c <- get
              modify (+1)
              pure $ Just c

        getParentRefs :: Applicative f => Int -> f IntSet
        getParentRefs = pure . parentRefs . (refs !)

        -- Gets the child refs for an index and replaces the edge datum with ().
        getChildRefs :: Applicative f => Int -> f (IntMap ())
        getChildRefs = pure . ($> ()) . childRefs . (refs !)

        -- Nice stuff to have in scope.
        refs       = references dag
        dagSize    = length refs
        notLeafRef = not . null . childRefs . (refs !)

        newRefs :: State Int (Vector (IndexData () (Maybe Int)))
        newRefs = V.generateM dagSize g
          where
            g :: Int -> State Int (IndexData () (Maybe Int))
            g i = liftA3 IndexData (getLeafIndex i) (getParentRefs i) (getChildRefs i)


reifyDAGWithContext :: Int -> ReferenceDAG () () (Maybe Int) -> UnReifiedCharacterDAG -> CharacterDAG
reifyDAGWithContext leafCount maskDAG (PDAG m dag) = PDAG2 newRDAG m
  where
    newRDAG =
      dag & _references .~ newRefs
          & _graphData  %~ setDefaultMetadata

    buildLeafNodeAssignments = nodeDecoration <$> references maskDAG

    dagSize = length $ references dag

    newRefs = V.generate dagSize g
      where
        g i = IndexData <$> const (memo ! i) <*> parentRefs <*> childRefs $ references dag ! i

    memo = V.generate dagSize g
      where
        g i = newNode
          where
            indexData = references dag ! i
            newNode =
                PNode2
                { resolutions          = resInfo
                , nodeDecorationDatum2 = nodeDecorationDatum $ nodeDecoration indexData
                }
            resMeta =
                    ResolutionMetadata
                    { totalSubtreeCost       = 0
                    , localSequenceCost      = 0
                    , subtreeEdgeSet         = mempty
                    , leafSetRepresentation  = bv
                    , subtreeRepresentation  = ns
                    , topologyRepresentation = mempty
                    }
            resInfo = pure
                    ResInfo
                    { resolutionMetadata = resMeta
                    , characterSequence  = sequenceDecoration $ nodeDecoration indexData
                    }

            (bv, ns) =
              case buildLeafNodeAssignments ! i of
                Just n  -> ( leafCount `singletonSubtreeLeafSet` n
                           , singletonNewickSerialization n
                           )
                Nothing ->
                  case IM.keys $ childRefs indexData of
                    x:xs -> ( foldr1 (.|.) $ (^. _leafSetRepresentation) . NE.head . resolutions . (memo !) <$> (x:|xs)
                            , foldMap1      ((^. _subtreeRepresentation) . NE.head . resolutions . (memo !)) (x:|xs)
                            )
                    []   -> error "Never occurs."
