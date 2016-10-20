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

    initialAccumulator = (-1, Nothing, mempty, mempty)
    (_, _, rootIndices, resultMap) = g initialAccumulator origin
    g acc@(counter, previousContext, currentRoots, currentMap) currentValue = result
      where
        result = (\x -> trace ("Result " <> show currentIndex <>": " <> show x) x) $
                 ( cCounter
                 , previousContext, currentRoots <> pRoots <> cRoots <> localRoots
                 , cMap <> mapWithLocalChildren <> mapWithLocalParents <> mapWithLocalValues
                 )
        
        (fullParentPairs, newDatum, fullChildPairs) =  (\x -> trace ("Application " <> show currentIndex <> ": " <> show x) x) $ f currentValue
        (omittedParentPairs, parentPairs) = omitOriginPath fullParentPairs
        (omittedChildPairs , childPairs ) = omitOriginPath fullChildPairs

        currentIndex   = counter + 1
        currentContext = Just (currentIndex, currentValue)

        omitOriginPath =
            case previousContext of
              Just (_,previousValue) -> span ((== previousValue) . snd)
              Nothing -> \x -> ([],x)

        parentResursiveResult       = (\x -> trace ("parentRecursiveResult " <> show currentIndex <> ": " <> show x) x) $
                                      scanr (\e a -> second (g (snd a)) e) (toEnum (-1), (currentIndex, currentContext, currentRoots, currentMap)) $ parentPairs
        (pCounter, _, pRoots, pMap) = snd $ head parentResursiveResult
        childResursiveResult        = (\x -> trace ("childRecursiveResult: " <> show currentIndex <>": " <> show x) x) $
                                      scanr (\e a -> second (g (snd a)) e) (toEnum (-1), (pCounter, currentContext, pRoots, pMap)) $ childPairs
        (cCounter, _, cRoots, cMap) = (\x -> trace ("childResultHead: " <> show currentIndex <>": " <> show x) x) $
                                      snd $ head childResursiveResult

        myCounter = cCounter + 1

        mapWithLocalParents = (\x -> trace ("totalParentMap: " <> show counter <> ": " <> show x) x) $ foldMap h $ init parentResursiveResult
          where
            h (_,(c,_,_,_)) = IM.insertWith insWith cCounter (IS.singleton c, newDatum, mempty) cMap
              where
                insWith (niSet, _, niMap) (oiSet, dec, oiMap) = (niSet <> oiSet, dec, oiMap <> niMap)

        mapWithLocalChildren = (\x -> trace ("totalChildMap: " <> show counter <>": " <> show x) x) $ foldMap h $ init childResursiveResult
          where
            h (e,(c,_,_,_)) = IM.insertWith insWith cCounter (mempty, newDatum, IM.singleton c e) cMap
              where
                insWith (niSet, _, niMap) (oiSet, dec, oiMap) = (niSet <> oiSet, dec, oiMap <> niMap)

        -- Do stuff with current context here!
        mapWithLocalValues  = (\x -> trace ("localValuesMap: " <> show counter <>": " <> show x) x) $
                              IM.singleton currentIndex
                            ( otherParents  <> foldMap (\(_,(c,_,_,_)) -> IS.singleton c  ) (init parentResursiveResult)
                            , newDatum
                            , otherChildren <> foldMap (\(e,(c,_,_,_)) -> IM.singleton c e) (init childResursiveResult)
                            )
          where
            otherParents =
              case previousContext of
                Nothing -> mempty
                Just (previousIndex,_) ->
                  if null omittedParentPairs
                  then mempty
                  else IS.singleton previousIndex
                  
            otherChildren = (\x -> trace ("otherChildren: " <> show counter <>": " <> show x) x) $
              case previousContext of
                Nothing -> mempty
                Just (previousIndex, previousValue) ->
                  if null omittedChildPairs
                  then mempty
                  else
                    let e = fst . head . filter ((==currentValue) . snd) . (\(_,_,x) -> x) $ f previousValue
                    in  IM.singleton previousIndex e

        localRoots
          | null fullParentPairs = IS.singleton cCounter
          | otherwise            = mempty


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

    maxParentWidth = maximum $ length <$> shownTrimmedParentRefs

    maxChildWidth  = maximum $ length <$> shownTrimmedChildRefs

    maxIndexWidth  = length . show . pred . length $ references dag

    shownDataLines = zipWithKey f shownPaddedParentRefs shownPaddedChildRefs
      where
        f i p c = "  " <> unwords [ pad maxIndexWidth $ show i, p, c]

    listShow = (\x -> "{" <> x <> "}") . intercalate "," . fmap show

    pad n    []  = replicate n ' '
    pad 0    xs  = xs
    pad n (x:xs) = x : pad (n-1) xs
