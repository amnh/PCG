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
import qualified Data.Map as M
import Control.Lens
import Bio.Graph.PhylogeneticDAG
import Bio.Graph.ReferenceDAG
import Data.NodeLabel
import Data.Vector (Vector, (!))
import qualified Data.Vector as V
import Prelude hiding (zip, length)
import Data.Foldable
import Data.Monoid (First(..))
import Bio.Graph.Node
import Bio.Graph.ReferenceDAG.Internal (IndexData)
import Bio.Sequence
import qualified Data.List.NonEmpty as NE
import Bio.Graph.ReferenceDAG.Utility (incrementRefVector)
import Data.IntMap.Lazy (keys)
import qualified Data.IntSet as IS


type Name = NodeLabel
type CharacterIndexData e n u v w x y z = (IndexData e (PhylogeneticNode (CharacterSequence u v w x y z) n))

getNamedContext :: forall m e n u v w x y z . (Ord n) => PhylogeneticDAG m e n u v w x y z -> [n] -> M.Map n Int
getNamedContext dag names = foldMap f names
  where
    f :: n -> M.Map n Int
    f n = case (dag `getIndexFromName` n) of
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
    getNameFromIndexData :: (IndexData e (PhylogeneticNode (CharacterSequence u v w x y z) n)) -> n
    getNameFromIndexData indData =
      indData ^. _nodeDecoration . _nodeDecorationDatum
    isName :: (CharacterIndexData e n u v w x y z, Int) -> Bool
    isName = \(c, _) -> (getNameFromIndexData c) == name
    ind = getFirst . foldMap (First . Just) . V.filter isName $ refsWithInds
  in
    snd <$> ind

substituteSingle
  :: (Ord n)
  => M.Map n Int
  -> (n, PhylogeneticDAG m e n u v w x y z)
  -> PhylogeneticDAG m e n u v w x y z -> PhylogeneticDAG m e n u v w x y z
substituteSingle namedContext (nodeName, subGraph) totalGraph =
  let relevantIndexOpt = nodeName `M.lookup` namedContext in
  case relevantIndexOpt of
    Nothing  -> totalGraph
    Just ind ->

      let
        rootInd          = NE.head $ subGraph ^. _phylogeneticForest . _rootRefs
        subReferences    = subGraph ^. _phylogeneticForest .  _references
        sizeOfSubGraph   = length subReferences
        rootChildInds    = (subReferences ! rootInd) ^. _childRefs
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
        removeRootNode = undefined

        incTotalRefNewChild
          = incrementedTotalRef
          & ix ind -- this is still the old index!
          . _childRefs
          .~ rootChildData
      in
        undefined


substitute :: M.Map n Int -> [(n, PhylogeneticDAG m e n u v w x y z)] -> PhylogeneticDAG m e n u v w x y z -> PhylogeneticDAG m e n u v w x y z
substitute = undefined

-- Function taken from: http://hackage.haskell.org/package/ilist-0.3.1.0/docs/Data-List-Index.html
deleteAt :: Int -> [a] -> [a]
deleteAt i ls
  | i < 0 = ls
  | otherwise = go i ls
  where
    go 0 (_:xs) = xs
    go n (x:xs) = x : go (n-1) xs
    go _ [] = []
{-# INLINE deleteAt #-}
