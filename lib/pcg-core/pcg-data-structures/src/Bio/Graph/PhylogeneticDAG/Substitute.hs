-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Graph.PhylogeneticDAG.Substitute
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

{-# LANGUAGE ScopedTypeVariables #-}


module Bio.Graph.PhylogeneticDAG.Substitute where
import           Bio.Graph.PhylogeneticDAG.Internal
import           Bio.Graph.ReferenceDAG
import           Control.Lens hiding (_head)
import qualified Data.Map                           as M
import           Bio.Graph.Node
import           Bio.Graph.ReferenceDAG.Internal    (IndexData)
import           Bio.Graph.ReferenceDAG.Utility     (incrementRefVector)
import           Bio.Sequence
import           Data.Foldable
import           Data.IntMap.Lazy                   (IntMap, keys)
import qualified Data.IntMap.Lazy as IM
import Data.IntSet (IntSet)
import qualified Data.IntSet                        as IS
import           Data.Monoid                        (First (..))
import           Data.Vector                        (Vector, (!))
import qualified Data.Vector                        as V
import           Prelude                            hiding (length, zip)
import Data.List.Utility
import Control.Monad.State.Strict (State, MonadState(..))

import Debug.Trace
import Data.Key (foldrWithKeyM)


type CharacterIndexData e n u v w x y z = (IndexData e (PhylogeneticNode (CharacterSequence u v w x y z) n))

type PhylogeneticRefDAG e n u v w x y z =
  ReferenceDAG
    (PostorderContextualData (CharacterSequence u v w x y z))
    e
    (PhylogeneticNode (CharacterSequence u v w x y z) n)

getNamedContext :: forall m e n u v w x y z . (Ord n) => PhylogeneticDAG m e n u v w x y z -> [n] -> M.Map n Int
getNamedContext dag = foldMap f
  where
    f :: n -> M.Map n Int
    f n = case dag `getIndexFromName` n of
          Nothing  -> mempty
          Just ind -> M.singleton n ind

getIndexFromName :: forall m e n u v w x y z . (Eq n)
  =>  PhylogeneticDAG m e n u v w x y z -> n -> Maybe Int
getIndexFromName dag name =
  let
    refs :: Vector (CharacterIndexData e n u v w x y z)
    refs = dag ^. (_phylogeneticForest . _references)

    l = length refs

    refsWithInds :: Vector (CharacterIndexData e n u v w x y z, Int)
    refsWithInds = V.zip refs (V.fromList [0..l])

    getNameFromIndexData
      :: IndexData e (PhylogeneticNode (CharacterSequence u v w x y z) n) -> n
    getNameFromIndexData indData =
      indData ^. _nodeDecoration . _nodeDecorationDatum

    isName :: (CharacterIndexData e n u v w x y z, Int) -> Bool
    isName (c, _) = getNameFromIndexData c == name

    ind = getFirst . foldMap (First . Just) . V.filter isName $ refsWithInds
  in
    snd <$> ind

substituteSingle
  :: forall m e n u v w x y z . (Ord n, Monoid n)
  => n
  -> PhylogeneticDAG m e n u v w x y z
  -> PhylogeneticDAG m e n u v w x y z
  -> State (M.Map n Int) (PhylogeneticDAG m e n u v w x y z)
substituteSingle nodeName subGraph totalGraph = do
  namedContext <- get
  let relevantIndexOpt = nodeName `M.lookup` namedContext in
    case relevantIndexOpt of
      Nothing  -> pure totalGraph
      Just subInd -> do
        currentNamedContext <- get
        let
          rootInd          = subGraph ^. _phylogeneticForest . _rootRefs . _head
          subReferences    = subGraph ^. _phylogeneticForest .  _references
          sizeOfSubGraph   = length subReferences
          sizeOfNewSubGraph = length subReferences - 1
          totalReferences  = totalGraph ^. _phylogeneticForest . _references
          incrementedTotalRef = incrementRefVector (sizeOfSubGraph - 1) totalReferences
          incrementedInd   = subInd + (sizeOfSubGraph - 1)
          rootChildData    = (subReferences ! rootInd) ^. _childRefs
          rootChildRefs    = keys rootChildData
          updateParentIndsSubRef
              = foldr
                  (\key refs -> refs & ix key . _parentRefs .~ IS.singleton incrementedInd)
                  subReferences
                  rootChildRefs
          updatedForDeletionSubNodes = decrementAfterIndex rootInd updateParentIndsSubRef
          removeRootNodeSub = deleteAtV rootInd updatedForDeletionSubNodes
          newSubGraphRefs   = removeRootNodeSub

          updatedRootChildData    = (updatedForDeletionSubNodes ! rootInd) ^. _childRefs
          incTotalRefNewChild
            = incrementedTotalRef
            & ix subInd -- this is still the old index!
            . _childRefs
            .~ updatedRootChildData
  
       -- This allows us to freely give temporary names to the leaves in the total graph which
       -- are then replaced when we perform the substitution
          oldRootName
            =  subReferences
            ^. (ix rootInd
            . _nodeDecoration
            . _nodeDecorationDatum
               )
            
  
          renamedUpdatedTotalRefs
            = incTotalRefNewChild
            & ix subInd
            . _nodeDecoration
            . _nodeDecorationDatum
            .~ oldRootName
  
          newTotalGraphRefs      = renamedUpdatedTotalRefs
          updatedReferenceVector = newSubGraphRefs <> newTotalGraphRefs
  
          totReferenceDAG = totalGraph ^. _phylogeneticForest
--          updatedRootRefs =   _rootRefs %~ (fmap (\n -> n + sizeOfNewSubGraph))

          newReferenceDAG
            = totReferenceDAG
        -- TODO: fix this -->  & _graphData .~ updateGraphMetadata
            &  _references .~ updatedReferenceVector
            &  _rootRefs %~ (fmap (\n -> n + sizeOfNewSubGraph))
          updatedNamedContext = fmap (\n -> n + sizeOfNewSubGraph) namedContext
          in
            do
              pure updatedNamedContext
              pure $ totalGraph & _phylogeneticForest .~ newReferenceDAG
  

substituteDAGs :: (Ord n, Show e, Show n, Monoid n) => M.Map n (PhylogeneticDAG m e n u v w x y z) -> PhylogeneticDAG m e n u v w x y z -> State (M.Map n Int) (PhylogeneticDAG m e n u v w x y z)
substituteDAGs namedSubGraphs totalGraph
  = --(\x -> trace (unlines
    --[ "namedContext" <> (show namedContext)
    --, "totalGraph: " <> f totalGraph
    --, "subgraphs: "  <> (fold $ fmap f (M.elems namedSubGraphs))
    --, "finalRes: "   <> (f x)
    --]) x) $
  foldrWithKeyM substituteSingle totalGraph namedSubGraphs


deleteAt :: Int -> [a] -> [a]
deleteAt i ls
  | i < 0 = ls
  | otherwise = go i ls
  where
    go 0 (_:xs) = xs
    go n (x:xs) = x : go (n-1) xs
    go _ []     = []
{-# INLINE deleteAt #-}

deleteAtV :: Int -> Vector a -> Vector a
deleteAtV i = V.fromList . deleteAt i . toList

-- |
-- This function takes an index of a deleted node and appropriately
-- decrements all index information of indices greater than
-- this node. This is to be used /before/ the node has been deleted.
decrementAfterIndex :: Int -> Vector (IndexData e n) -> Vector (IndexData e n)
{-# inline decrementAfterIndex #-}
decrementAfterIndex ind = fmap updateIndexData
  where
    f :: Int -> Int
    {-# INLINE f #-}
    f n = case n <= ind of
      True  -> n
      False -> n - 1

    updateParentRefs :: IntSet -> IntSet
    {-# INLINE updateParentRefs #-}
    updateParentRefs = IS.map f

    updateChildRefs :: IntMap a -> IntMap a
    {-# INLINE updateChildRefs #-}
    updateChildRefs = IM.mapKeys f

    updateIndexData :: IndexData e' n' -> IndexData e' n'
    {-# INLINE updateIndexData #-}
    updateIndexData i = i & _parentRefs %~ updateParentRefs
                          & _childRefs  %~ updateChildRefs

--f :: (Show e) => (PhylogeneticDAG m e n u v w x y z) -> String
--f dag = show $ fmap g (dag ^. _phylogeneticForest . _references)
--  where
--    g :: IndexData e' n' -> (IS.IntSet, [Int])
--    g ind = (ind ^. _parentRefs, keys $ ind ^. _childRefs)

