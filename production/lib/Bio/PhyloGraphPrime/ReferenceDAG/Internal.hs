-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.PhyloGraphPrime.ReferenceDAG.Internal
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances, FunctionalDependencies, GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}

module Bio.PhyloGraphPrime.ReferenceDAG.Internal where

import           Bio.PhyloGraphPrime.Component
import           Data.Bifunctor
import           Data.Foldable
import           Data.List.NonEmpty        (NonEmpty)
import qualified Data.List.NonEmpty as NE
import           Data.IntMap               (IntMap)
import qualified Data.IntMap        as IM
import           Data.IntSet               (IntSet)
import qualified Data.IntSet        as IS
import           Data.Key
import           Data.Monoid               ((<>))
import           Data.MonoTraversable
import           Data.Vector               (Vector)
import qualified Data.Vector        as V
import           Data.Vector.Instances     ()
import           Prelude            hiding (lookup)


-- |
-- A constant time access representation of a directed acyclic graph.
-- 
data ReferenceDAG e n
   = RefDAG
   { references :: Vector (IndexData e n)
   , rootRefs   :: NonEmpty Int
   , graphData  :: GraphData
   }


data IndexData e n
   = IndexData
   { nodeDecoration :: n
   , parentRefs     :: IntSet
   , childRefs      :: IntMap e
   }


-- | Annotations which are global to the graph
data GraphData
   = GraphData
   { cost :: Double
   }


newtype NodeRef = NR Int deriving (Eq, Enum)
                   

-- | (✔)
instance Bifunctor ReferenceDAG where

    bimap f g dag =
        RefDAG
        { references = h <$> references dag
        , rootRefs   = rootRefs  dag
        , graphData  = graphData dag
        }
      where
        h (IndexData node parentRefs' childRefs') = IndexData (g node) parentRefs' $ f <$> childRefs'


-- | (✔)
instance Functor (ReferenceDAG e) where

    fmap f dag =
        RefDAG
        { references = g <$> references dag
        , rootRefs   = rootRefs  dag
        , graphData  = graphData dag
        }
      where
        g (IndexData node parentRefs' childRefs') = IndexData (f node) parentRefs' childRefs'


-- | Build the graph functionally from a generating function.
unfoldDAG :: Eq b => (b -> ([(e,b)], n, [(e,b)])) -> b -> ReferenceDAG e n
unfoldDAG f origin =
    RefDAG
    { references = referenceVector
    , rootRefs   = NE.fromList $ otoList rootIndices
    , graphData  = GraphData 0
    }
  where
    referenceVector = V.fromList . fmap h $ toList resultMap
      where
        h (iSet, nDatum, iMap) =
            IndexData
            { nodeDecoration = nDatum
            , parentRefs     = iSet
            , childRefs      = iMap
            }
    
    initialAccumulator :: (Int, b, IntSet, IntMap (IntSet, n, IntMap e))
    initialAccumulator = (0, origin, mempty, mempty)
    (_, _, rootIndices, resultMap) = g initialAccumulator origin
    g acc@(counter, previousValue, currentRoots, currentMap) currentValue = result
      where
        result = (cCounter + 1, currentValue, cRoots <> localRoots, currentMap <> mapWithLocalChildren <> mapWithLocalParents)
        
        (parentPairs, newDatum, childPairs) = resultFilter $ f currentValue
        resultFilter (x,y,z) = (filter' x, y, filter' z)
          where
            filter' = filter ((/= previousValue) . snd)

        parentResursiveResult       = scanr (\e a -> second (g (snd a)) e) (undefined, acc) $ parentPairs
        (pCounter, _, pRoots, pMap) = snd $ head parentResursiveResult
        childResursiveResult        = scanr (\e a -> second (g (snd a)) e) (undefined, (pCounter, currentValue, pRoots, pMap)) $ childPairs
        (cCounter, _, cRoots, cMap) = snd $ head childResursiveResult

        mapWithLocalChildren = foldMap h childResursiveResult
          where
            h (e,(c,_,_,_)) = IM.insertWith insWith cCounter (mempty, newDatum, IM.singleton c e) cMap
              where
                insWith (niSet, _, niMap) (oiSet, dec, oiMap) = (niSet <> oiSet, dec, oiMap <> niMap)

        mapWithLocalParents = foldMap h parentResursiveResult
          where
            h (_,(c,_,_,_)) = IM.insertWith insWith cCounter (IS.singleton c, newDatum, mempty) cMap
              where
                insWith (niSet, _, niMap) (oiSet, dec, oiMap) = (niSet <> oiSet, dec, oiMap <> niMap)

        localRoots
          | null parentPairs = IS.singleton cCounter
          | otherwise        = mempty



instance PhylogeneticComponent (ReferenceDAG e n) NodeRef e n where

    parents   i dag = fmap toEnum . otoList . parentRefs $ references dag V.! fromEnum i
 
    children  i dag = fmap toEnum . IM.keys . childRefs  $ references dag V.! fromEnum i

    roots           = fmap toEnum . rootRefs

    leaves          = foldMapWithKey f . references
      where
        f i x
          | null $ childRefs x = mempty
          | otherwise          = [toEnum i]

    nodeCount       = length . references

    nodeDatum i dag = nodeDecoration $ references dag V.! fromEnum i

    edgeDatum (i,j) dag =  fromEnum j `lookup` childRefs (references dag V.! fromEnum i)

    -- TODO: Broken
    isComponentNode i dag = olength ps > 2
      where
        ps = parentRefs $ references dag V.! fromEnum i

    -- TODO: Broken
    isNetworkNode i dag = olength ps > 2
      where
        ps = parentRefs $ references dag V.! fromEnum i

    isTreeNode i dag = olength ps == 1 && length cs == 2
      where
        iPoint = references dag V.! fromEnum i 
        ps = parentRefs iPoint
        cs = childRefs  iPoint

    isLeafNode i dag =  null . childRefs  $ references dag V.! fromEnum i

    isRootNode i dag = onull . parentRefs $ references dag V.! fromEnum i

    -- TODO: Broken
    networkResolutions dag = pure dag


instance PhylogeneticNetwork (ReferenceDAG e n) NodeRef e n where

    root = toEnum . NE.head . rootRefs
  
    -- TODO: Broken
    treeResolutions dag = pure dag


instance PhylogeneticTree (ReferenceDAG e n) NodeRef e n where

    parent i dag = fmap toEnum . headMay . otoList . parentRefs $ references dag V.! fromEnum i
