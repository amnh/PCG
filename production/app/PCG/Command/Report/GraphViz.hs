-----------------------------------------------------------------------------
-- |
-- Module      :  PCG.Command.Report.Graphviz
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Functionality to output a graphviz format from a Graph
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts #-}

module PCG.Command.Report.GraphViz where

import           Bio.Graph
import           Bio.Graph.Node
import           Bio.Graph.PhylogeneticDAG
import           Bio.Graph.ReferenceDAG.Internal
import           Data.Bifunctor
import           Data.Foldable
import           Data.GraphViz.Printing hiding ((<>)) -- Seriously, why is this redefined?
import           Data.GraphViz.Types
import           Data.GraphViz.Types.Graph
import qualified Data.IntMap            as IM
import           Data.Key               hiding (zipWith)
import           Data.Semigroup
import           Data.String
import qualified Data.Text.Lazy         as L

--import Debug.Trace


generateDotFile :: GraphState -> String
generateDotFile = (<> "\n") . L.unpack . renderDot . toDot . getDotGraph


getDotGraph :: GraphState -> DotGraph GraphID
getDotGraph = uncurry mkGraph . either noCharGraph hasCharGraph
  where
    noCharGraph  :: TopologicalResult -> ([DotNode GraphID], [DotEdge GraphID])
    noCharGraph  = solutionDotContext rdag
    
    hasCharGraph :: DecoratedCharacterResult -> ([DotNode GraphID], [DotEdge GraphID])
    hasCharGraph = solutionDotContext pdag

    pdag (PDAG2 dag) = getDotContext $ nodeDecorationDatum2 <$> dag

    rdag = getDotContext

    solutionDotContext
      :: (a -> ([DotNode GraphID], [DotEdge GraphID]))
      -> PhylogeneticSolution a
      -> ([DotNode GraphID], [DotEdge GraphID])
    solutionDotContext g = mergeContexts . fmap (forestDotContext g) . phylogeneticForests

    forestDotContext
      :: (a -> ([DotNode GraphID], [DotEdge GraphID]))
      -> PhylogeneticForest a
      -> ([DotNode GraphID], [DotEdge GraphID])
    forestDotContext g (PhylogeneticForest dags) = mergeContexts $ g <$> dags


getDotContext :: Foldable f => ReferenceDAG d e (f String) -> ([DotNode GraphID], [DotEdge GraphID])
getDotContext dag = second mconcat . unzip $ foldMapWithKey f vec
  where
    vec = references dag

    toId :: Foldable f => Int -> f String -> GraphID
    toId i x =
      case toList x of
        []  -> Num $ Int i
        s:_ -> Str $ fromString s

    f :: Foldable f => Int -> IndexData e (f String) -> [(DotNode GraphID, [DotEdge GraphID])]
    f k v = [ (toDotNode, toDotEdge <$> kidRefs) ]
      where
        datum       = nodeDecoration v
        nodeId      = toId k datum
        kidRefs     = IM.keys $ childRefs v
        toDotNode   = DotNode nodeId []
        toDotEdge x = DotEdge (toId x (nodeDecoration $ vec ! x)) nodeId []


mergeContexts :: Foldable f => f ([DotNode GraphID], [DotEdge GraphID]) -> ([DotNode GraphID], [DotEdge GraphID])
mergeContexts = bimap mconcat mconcat . unzip . toList
