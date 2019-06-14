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
import           Control.Lens
import qualified Data.Map                           as M
import           Bio.Graph.Node
import           Bio.Graph.ReferenceDAG.Internal    (IndexData)
import           Bio.Graph.ReferenceDAG.Utility     (incrementRefVector)
import           Bio.Sequence
import           Bio.Sequence.Metadata
import           Data.Foldable
import           Data.IntMap.Lazy                   (keys)
import qualified Data.IntSet                        as IS
import qualified Data.List.NonEmpty                 as NE
import           Data.Monoid                        (First (..))
import           Data.Vector                        (Vector, (!))
import qualified Data.Vector                        as V
import           Prelude                            hiding (length, zip)


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
  :: forall m e n u v w x y z . (Ord n)
  => M.Map n Int
  -> n
  -> PhylogeneticDAG m e n u v w x y z
  -> PhylogeneticDAG m e n u v w x y z -> PhylogeneticDAG m e n u v w x y z
substituteSingle namedContext nodeName subGraph totalGraph =
  let relevantIndexOpt = nodeName `M.lookup` namedContext in
  case relevantIndexOpt of
    Nothing  -> totalGraph
    Just ind ->

      let
        rootInd          = NE.head $ subGraph ^. _phylogeneticForest . _rootRefs
        subReferences    = subGraph ^. _phylogeneticForest .  _references
        sizeOfSubGraph   = length subReferences
        totalReferences  = totalGraph ^. _phylogeneticForest . _references
        incrementedTotalRef = incrementRefVector (sizeOfSubGraph - 1) totalReferences
        incrementedInd   = ind + (sizeOfSubGraph - 1)
        rootChildData    = (subReferences ! rootInd) ^. _childRefs
        rootChildRefs    = keys rootChildData
        updateParentIndsSubRef
          = foldr
              (\key refs -> refs & ix key . _parentRefs .~ IS.singleton incrementedInd)
              subReferences
              rootChildRefs
        removeRootNodeSub = deleteAtV rootInd updateParentIndsSubRef
        newSubGraphRefs   = removeRootNodeSub

        incTotalRefNewChild
          = incrementedTotalRef
          & ix ind -- this is still the old index!
          . _childRefs
          .~ rootChildData

        newTotalGraphRefs = incTotalRefNewChild
        updatedReferenceVector = newSubGraphRefs <> newTotalGraphRefs

        subMetadataSequence = subGraph ^. _columnMetadata
        totMetadataSequence = totalGraph ^. _columnMetadata
        totReferenceDAG = totalGraph ^. _phylogeneticForest
        newReferenceDAG :: PhylogeneticRefDAG e n u v w x y z
        newReferenceDAG = totReferenceDAG
      -- TODO: fix this -->  & _graphData .~ updateGraphMetadata
                        & _references .~ updatedReferenceVector
        newMetadataSequence = substituteMetadataSequence ind subMetadataSequence totMetadataSequence
      in
        PDAG2 { phylogeneticForest = newReferenceDAG , columnMetadata = newMetadataSequence }


substituteDAGs :: (Ord n) => M.Map n Int -> M.Map n (PhylogeneticDAG m e n u v w x y z) -> PhylogeneticDAG m e n u v w x y z -> PhylogeneticDAG m e n u v w x y z
substituteDAGs namedContext namedSubGraphs totalGraph
  = M.foldrWithKey (substituteSingle namedContext) totalGraph namedSubGraphs

-- Function taken from: http://hackage.haskell.org/package/ilist-0.3.1.0/docs/Data-List-Index.html
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



