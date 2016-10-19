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
import           Data.List                 (intercalate)
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

import Debug.Trace (trace)

-- |
-- A constant time access representation of a directed acyclic graph.
-- 
data ReferenceDAG e n
   = RefDAG
   { references :: Vector (IndexData e n)
   , rootRefs   :: NonEmpty Int
   , graphData  :: GraphData
   }


-- |
-- A labeled record for each "node" in the graph containing the node decoration,
-- a set of parent references, and a set of child references with edge decorations.
data IndexData e n
   = IndexData
   { nodeDecoration :: n
   , parentRefs     :: IntSet
   , childRefs      :: IntMap e
   } deriving (Show)


-- | Annotations which are global to the graph
data GraphData
   = GraphData
   { cost :: Double
   }


-- |
-- A reference to a node within the 'ReferenceDAG'.
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


-- | (✔)
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


-- | (✔)
instance PhylogeneticNetwork (ReferenceDAG e n) NodeRef e n where

    root = toEnum . NE.head . rootRefs
  
    -- TODO: Broken
    treeResolutions dag = pure dag


-- | (✔)
instance PhylogeneticTree (ReferenceDAG e n) NodeRef e n where

    parent i dag = fmap toEnum . headMay . otoList . parentRefs $ references dag V.! fromEnum i


-- | (✔)
instance {- (Show e, Show n) => -} Show (ReferenceDAG e n) where

    show = referenceRendering 


-- TODO: Broken!
-- TODO: Preorder counter incrementation from origin (pushed down)
-- | Build the graph functionally from a generating function.
unfoldDAG :: (Eq b, Show b, Show e, Show n, Enum e) => (b -> ([(e,b)], n, [(e,b)])) -> b -> ReferenceDAG e n
unfoldDAG f origin =
    RefDAG
    { references = (\x -> trace (show x) x) referenceVector
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
    
    initialAccumulator = (0, origin, mempty, mempty)
    (_, _, rootIndices, resultMap) = g initialAccumulator origin
    g acc@(_counter, previousValue, _currentRoots, currentMap) currentValue = result
      where
        result = (\x -> trace ("Result " <> show _counter <>": " <> show x) x) $
                 (cCounter + 1, currentValue, cRoots <> localRoots, currentMap <> mapWithLocalChildren <> mapWithLocalParents <> mapWithLocalValues)
        
        (parentPairs, newDatum, childPairs) =  (\x -> trace ("Application: " <> show x) x) $ resultFilter $ f currentValue
        resultFilter (x,y,z) = (filter' x, y, filter' z)
          where
            filter' = filter ((/= previousValue) . snd)

        parentResursiveResult       = (\x -> trace (show x) x) $ scanr (\e a -> second (g (snd a)) e) (toEnum (-1), acc) $ parentPairs
        (pCounter, _, pRoots, pMap) = snd $ head parentResursiveResult
        childResursiveResult        = (\x -> trace (show x) x) $ scanr (\e a -> second (g (snd a)) e) (toEnum (-1), (pCounter, currentValue, pRoots, pMap)) $ childPairs
        (cCounter, _, cRoots, cMap) = snd $ head childResursiveResult

        mapWithLocalChildren = (\x -> trace ("totalChildMap " <> show _counter <>": " <> show x) x) $ foldMap h $ init childResursiveResult
          where
            h (e,(c,_,_,_)) = IM.insertWith insWith cCounter (mempty, newDatum, IM.singleton c e) cMap
              where
                insWith (niSet, _, niMap) (oiSet, dec, oiMap) = (niSet <> oiSet, dec, oiMap <> niMap)

        mapWithLocalParents = (\x -> trace ("totalParentMap: " <> show _counter <> ": " <> show x) x) $ foldMap h $ init parentResursiveResult
          where
            h (_,(c,_,_,_)) = IM.insertWith insWith cCounter (IS.singleton c, newDatum, mempty) cMap
              where
                insWith (niSet, _, niMap) (oiSet, dec, oiMap) = (niSet <> oiSet, dec, oiMap <> niMap)

        mapWithLocalValues  = IM.singleton cCounter
                            ( foldMap (\(_,(c,_,_,_)) -> IS.singleton c  ) parentResursiveResult
                            , newDatum
                            , foldMap (\(e,(c,_,_,_)) -> IM.singleton c e) childResursiveResult
                            )

        localRoots
          | null parentPairs = IS.singleton cCounter
          | otherwise        = mempty


-- |
-- Renders the 'ReferenceDAG' without showing the node or edge decorations.
-- Displays a multi-line, tabular reference map of the 'ReferenceDAG'. 
referenceRendering :: ReferenceDAG e n -> String
referenceRendering dag = unlines $ [shownRootRefs] <> toList shownDataLines
  where
    shownRootRefs   = listShow . toList $ rootRefs dag
    
    shownRefs       = f <$> references dag
      where
        f (IndexData _ pRefs cRefs) = (listShow $ otoList pRefs, listShow $ IM.keys cRefs)

    shownTrimmedParentRefs = fst <$> shownRefs
    
    shownTrimmedChildRefs  = snd <$> shownRefs

    shownPaddedParentRefs  = pad maxParentWidth <$> shownTrimmedParentRefs
    
    shownPaddedChildRefs   = pad maxChildWidth  <$> shownTrimmedChildRefs

    maxParentWidth  = maximum $ length <$> shownTrimmedParentRefs

    maxChildWidth   = maximum $ length <$> shownTrimmedChildRefs

    maxIndexWidth   = length . show . pred . length $ references dag

    shownDataLines = zipWithKey f shownPaddedParentRefs shownPaddedChildRefs
      where
        f i p c = unwords [ show (pad maxIndexWidth $ show i), p, c]

    listShow = (\x -> "{" <> x <> "}") . intercalate "," . fmap show

    pad n    []  = replicate n ' '
    pad 0    xs  = xs
    pad n (x:xs) = x : pad (n-1) xs
