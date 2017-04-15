------------------------------------------------------------------------------
-- |
-- Module      :  Bio.PhyloGraphPrime.PhylogeneticDAG.DynamicCharacterRerooting
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

module Bio.PhyloGraphPrime.PhylogeneticDAG.DynamicCharacterRerooting
  ( assignOptimalDynamicCharacterRootEdges
  ) where

import           Bio.Character.Decoration.Additive
import           Bio.Character.Decoration.Dynamic
import           Bio.Sequence
import           Bio.PhyloGraphPrime.Node
import           Bio.PhyloGraphPrime.PhylogeneticDAG.Internal
import           Bio.PhyloGraphPrime.ReferenceDAG.Internal
import           Control.Applicative
import           Control.Arrow             ((&&&))
import           Control.Lens
import           Control.Monad.State.Lazy
import           Data.Foldable
import qualified Data.IntMap        as IM
import           Data.Key
import           Data.List.NonEmpty        (NonEmpty( (:|) ))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map           as M
import           Data.Maybe
import           Data.MonoTraversable
import           Data.Semigroup
import qualified Data.Vector        as V
import           Prelude            hiding (lookup, zipWith)


-- |
-- For every edge in the component:
--
-- * If the edge is *not* a network edge:
--
-- The re-rooting candidate cost for that edge (for a character) is the minimum
-- cost of the cartesian product of the resolutions of the adjacent nodes.
--
-- * If the edge *is* a network edge:
--
-- The re-rooting candidate cost for that edge (for a character) is the minimum
-- cost of the cartesian product of the resolutions of the adjacent nodes minus any
-- resolutions that contain the "incident" network edge contained on the current
-- network edge.

assignOptimalDynamicCharacterRootEdges
  :: ( HasCharacterCost m Word
     , HasCharacterCost i Word
     , HasCharacterCost f Word
     , HasCharacterCost a Word
     , HasCharacterCost d Word
     , HasCharacterWeight m Double
     , HasCharacterWeight i Double
     , HasCharacterWeight f Double
     , HasCharacterWeight a Double
     , HasCharacterWeight d Double
     , HasTraversalLocus  d (Maybe TraversalLocusEdge)
     , Show m
     , Show i
     , Show c
     , Show f
     , Show a
     , Show d
     ) --x, Ord x, Show x)
  => (d -> [d] -> d)
  -> PhylogeneticDAG2 e n m i c f a d
  -> PhylogeneticDAG2 e n m i c f a d
assignOptimalDynamicCharacterRootEdges extensionTransformation (PDAG2 inputDag) = PDAG2 updatedDag
  where

    -- Step 1: Construct a hashmap of all the edges.
    unrootedEdges = rootEdgeReferences <> otherUnrootedEdges
    
    -- Step 2: Create a lazy memoized hashmap of the edge costs for each dynmaic character.

    edgeCostMapping = referenceEdgeMapping

    -- Step 3: For each dynamic character, find the minimal cost edge(s).
    minimalCostSequence = sequenceOfEdgesWithMinimalCost
    
    -- Step 4: Update the dynamic character decoration's cost & add an edge reference.
    updatedDag = inputDag { references = refVec V.// toList modifiedRootRefs }


    -- These are the edges of the DAG which might, not including the current root edge,
    -- which maybe be the optimal root for a given dynamic character.
    otherUnrootedEdges :: [EdgeReference]
    otherUnrootedEdges = foldMapWithKey f refVec
      where
        f i n
          -- Don't consider edges from a root node, as the edges are "artificial" in an unrooted context.
          | i `elem` rootRefs inputDag = []
          | otherwise                  = fmap (\e -> (i,e)) . IM.keys $ childRefs n

    rootEdgeReferences = foldMap f $ rootRefs inputDag
      where
        f i =
          case IM.keys . childRefs $ refVec ! i of
            []    -> []
            [_]   -> []
            x:y:_ -> [(x,y)]

    refVec = references inputDag

{-
    referenceEdgeMapping :: HashMap EdgeReference IncidentEdges
    referenceEdgeMapping = HM.fromList $ foldMap f otherUnrootedEdges <> foldMap g rootEdgeReferences
      where
        f e@(i,j) = [(e, parRefs <> cldRefs)]
          where
            parRefs = ofoldMap (\k -> [(k,i)])           . parentRefs $ refVec ! i
            cldRefs =  foldMap (\k -> [(j,k)]) . IM.keys .  childRefs $ refVec ! j
        g e@(i,j) = [(e, lhsRefs <> rhsRefs)]
          where
            lhsRefs =  foldMap (\k -> [(i,k)]) . IM.keys .  childRefs $ refVec ! i
            rhsRefs = ofoldMap (\k -> [(j,k)]) . IM.keys .  childRefs $ refVec ! j
-}

--    referenceEdgeMapping :: HashMap EdgeReference (ResolutionCache (CharacterSequence u v w x y z))
    referenceEdgeMapping = foldMap f unrootedEdges
      where
        f e@(i,j) =
            case liftA2 (,) lhsContext rhsContext of
              Just (lhs, rhs) -> M.singleton e $ localResolutionApplication extensionTransformation lhs rhs
              Nothing         -> error errorContext
          where
            lhsContext = (i `lookup` contextualNodeDatum) >>= ((i,j) `lookup`)
            rhsContext = (j `lookup` contextualNodeDatum) >>= ((j,i) `lookup`)
            errorContext = unlines
                [ show e
                , show $ M.keys <$> contextualNodeDatum
                ]
    


--    rerootedEdgeContexts :: HashMap EdgeReference (ReRootedEdgeContext u v w x y z)
{-
    rerootedEdgeContexts = foldMap f unrootedEdges
      where
        f e@(i,j)
          | e `elem` rootEdgeReferences = HM.singleton e (getCache i, getRootByChildren e, getCache j) -- identity case
          | otherwise                   = undefined -- memoized reference case
          where
-}            
   
    getCache i = resolutions . nodeDecoration $ refVec ! i
{-
    getRootByChildren (i,j) = resolutions . nodeDecoration . fst . head $ NE.filter findMatchingChildren rootChildren
      where
        rootChildren = (id &&& IM.keys . childRefs) . (refVec !) <$> rootRefs inputDag
        findMatchingChildren (_,is) = i `elem` is && j `elem` is
-}
--    contextualNodeDatum :: Vector (Map EdgeReference (ResolutionCache (CharacterSequence u v w x y z)))
    contextualNodeDatum = V.generate (length refVec) f
      where
        f i
          | i `elem` rootRefs inputDag = mempty -- undefined
          | otherwise                  = parentEdgeValue <> childEdgeValues
          where
            parentRef
              | candidate `notElem` rootRefs inputDag = candidate
              | otherwise = sibling
              where
                candidate = head . otoList . parentRefs $ refVec ! i
                sibling   = head . filter (/=i) . IM.keys .  childRefs $ refVec ! candidate
                
            kidRefs          = IM.keys .  childRefs $ refVec ! i 
            parentEdgeValue  = M.singleton (i, parentRef) $ getCache i
            childEdgeValues  = foldMap deriveDirectionalDatum [ (x,y) | x <- kidRefs, y <- kidRefs, x /= y ]
            deriveDirectionalDatum (j, k) = M.singleton (i,j) relativeSubtreeDatumValue

              where
                childMemoizedSubstructure    = (contextualNodeDatum ! k        ) ! (        k, i)
                parentalMemoizedSubstructure = (contextualNodeDatum ! parentRef) ! (parentRef, i)
                relativeSubtreeDatumValue    = localResolutionApplication extensionTransformation parentalMemoizedSubstructure childMemoizedSubstructure

    rootRefWLOG  = NE.head $ rootRefs inputDag
    sequenceWLOG = fmap dynamicCharacters . toBlocks . characterSequence . NE.head $ getCache rootRefWLOG


    sequenceOfEdgesWithMinimalCost = foldMapWithKey1 f sequenceWLOG
      where
        f k v = V.generate (length v) g :| []
          where
            g i = result
              where
                result@(_minimumEdge, _minimumCost) = fromJust $ foldlWithKey h Nothing edgeIndexCostMapping
                edgeIndexCostMapping = fmap (fmap ((^. characterCost) . (! i) . dynamicCharacters . (! k) . toBlocks . characterSequence)) edgeCostMapping
                h acc e cs =
                    case acc of
                      Nothing      -> Just (e, c)
                      Just (_e', c') ->
                        if   c < c'
                        then Just (e, c)
                        else acc
                  where
                    c = minimum cs


    -- Step 4: Update the dynamic character decoration's cost & add an edge reference.
    modifiedRootRefs = (id &&& modifyRootCosts . (refVec !)) <$> rootRefs inputDag
      where
        modifyRootCosts idxData = idxData { nodeDecoration = nodeDatum }
          where
            node = nodeDecoration idxData
            nodeDatum =
                PNode2
                { resolutions          = f <$> resolutions node
                , nodeDecorationDatum2 = nodeDecorationDatum2 node
                }
        f resInfo = resInfo { characterSequence = modifiedSequence  }
          where
            modifiedSequence = fromBlocks . foldMapWithKey1 g . toBlocks $ characterSequence resInfo
        g k charBlock = pure $ charBlock { dynamicCharacters = modifiedDynamicChars }
          where
            modifiedDynamicChars = zipWith h (minimalCostSequence ! k) $ dynamicCharacters charBlock
            h (edgeVal, costVal) originalDec = originalDec
                                                 & characterCost  .~ costVal
                                                 & traversalLocus .~ Just edgeVal
