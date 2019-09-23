{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE AllowAmbiguousTypes    #-}

module Data.Graph.Build where

import Data.Foldable
import Data.Semigroup.Foldable
import Control.Monad
import Control.Lens hiding (indexed)
import qualified Data.List.NonEmpty as NE
import Data.Vector (Vector, fromList)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Graph.Type
import Data.Graph.NodeContext
import Data.Graph.Indices
import Data.Pair.Strict
import Data.Vector (indexed)
import Data.Graph.Sequence.Class
import Control.Parallel.Strategies
import Control.Parallel.Custom
import Data.Ord (comparing)
import Control.Arrow

class HasScore g c where
  score :: g -> c

class HasLeafDecoration t t' | t' -> t where
  hasLeafDecoration :: Lens' t t'

class HasTreeDecoration n n' | n' -> n where
  hasTreeDecoration :: Lens' n n'

class HasFinalCacheDecoration c c' | c' -> c where
  hasFinalCacheDecoration :: Lens' c c'

class
   ( HasLeafDecoration t t'
   , HasTreeDecoration n n'
   , HasFinalCacheDecoration c c'
   ) =>
  HasGraphDecoration g c c' t t' n n' e where
  decorate
    :: g Maybe c () n t -> g Identity c' e n' t'


fromFoldable1 :: Foldable1 f => f a -> [a]
fromFoldable1 = NE.toList . toNonEmpty

rootsFromInds :: MonadPlus m => [Int] -> Vector (RootIndexData (m n) ())
rootsFromInds = fromList . fmap (\i -> rootIndexData mzero (Left $ childInfo LeafTag i ()))


initialBuild
  :: ( Monad m
     , MonadPlus m
     , Foldable1 f
     )
  => f t -> c -> Graph m c () n t
initialBuild ts cache =
  let
    tsList    = fromFoldable1 ts
    len       = length tsList
    -- This is safe as the length must be at least 1
    inds      = [0..len - 1]
    tsListInd = zip tsList inds
    leafRefs  = fromList $ fmap (\(t, i) -> leafIndexData t (tagValue RootTag i)) tsListInd
    rootRefs  = rootsFromInds inds
  in
    Graph
      { leafReferences    = leafRefs
      , treeReferences    = mempty
      , networkReferences = mempty
      , rootReferences    = rootRefs
      , cachedData        = cache
      }



naiveWagnerBuild
  :: forall c c' e n n' t t' f charSeq.
     ( HasScore (Graph Identity c' e n' t') Double
     , HasGraphDecoration Graph c c' t t' n n' e
     , HasCharacterSequence e charSeq
     , HasCharacterSequence n charSeq
     , MetricSpace charSeq
     , MedianSpace charSeq
     , Foldable1 f
    )
  => f t -> c -> Graph Identity c' e n' t'
naiveWagnerBuild ts cache =
  case toNonEmpty ts of
      t:|[]  -> decorate $
                  Graph
                  { leafReferences
                      = fromList
                          [ leafIndexData t (tagValue RootTag 0)]
                  , treeReferences    = mempty
                  , networkReferences = mempty
                  , rootReferences
                      = fromList
                          [ rootIndexData
                              (mzero @Maybe)
                              (Left $ (childInfo LeafTag 0 ()))
                          ]
                  , cachedData = cache
                  }

      x:|[y] -> decorate $
                  Graph
                   { leafReferences
                      = fromList
                          [ leafIndexData x (tagValue RootTag 0)
                          , leafIndexData y (tagValue RootTag 0)
                          ]
                  , treeReferences    = mempty
                  , networkReferences = mempty
                  , rootReferences
                      = fromList
                          [ rootIndexData
                              mzero
                              (Right $ (childInfo LeafTag 0 ()) :!: (childInfo LeafTag 1 ()))
                          ]
                  , cachedData = cache
                  }
      x:|(y:z:xs) ->
        let initTree
              = decorate $ undefined
                    
--                  [ ( mempty        , wipeNode True  x, IM.fromList [(1,mempty), (4,mempty)] )
--                  , ( IS.singleton 0, wipeNode True  x, IM.fromList [(2,mempty), (3,mempty)] )
--                  , ( IS.singleton 1, wipeNode False x, mempty )
--                  , ( IS.singleton 1, wipeNode False y, mempty )
--                  , ( IS.singleton 0, wipeNode False z, mempty )
--                  ]
        in
          foldl' iterativeBuild initTree xs


iterativeBuild
  :: forall c c' e n n' t t' f charSeq .
     ( HasScore (Graph Identity c' e n' t') Double
     , HasGraphDecoration Graph c c' t t' n n' e
     , HasCharacterSequence e charSeq
     , HasCharacterSequence n charSeq
     , MetricSpace charSeq
     , MedianSpace charSeq
    )
  => Graph Identity c' e n' t'
  -> t
  -> Graph Identity c' e n' t'
iterativeBuild currentGraph tree = undefined
  where
    newEdge :: (Int, ChildIndex)
    newEdge =
      let
        treeNodes = currentGraph ^. _treeReferences
        treeNodesInd :: Vector (Int, (TreeIndexData (Identity n') e))
        treeNodesInd = indexed treeNodes
        extract = id *** view _right
        minimumNode =
          extract . minimumBy (comparing $ (view _right) . snd) $
          parmap rpar (id *** edgeCosts)
          treeNodesInd
      in
        undefined
    edgeCosts :: TreeIndexData (Identity n') e -> ChildIndex :!: Double
    edgeCosts treeInd =
        case comparing (view _right) leftEdge rightEdge of
        LT -> leftEdge
        GT -> rightEdge
        EQ -> leftEdge
      
      where
        leftEdge :: ChildIndex :!: Double
        leftEdge =
          let
            leftChildInfo = treeInd ^. _childInfo . _left
          in
            view _childIndex leftChildInfo
            :!:
            edgeCost (leftChildInfo ^. _edgeData)


        rightEdge :: ChildIndex :!: Double
        rightEdge =
          let
            rightChildInfo = treeInd ^. _childInfo . _right
          in
            view _childIndex rightChildInfo
            :!:
            edgeCost (rightChildInfo ^. _edgeData)


        edgeCost :: e -> Double
        edgeCost = undefined
