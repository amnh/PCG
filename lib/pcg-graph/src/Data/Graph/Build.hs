{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}


module Data.Graph.Build where

import           Control.Lens
import           Control.Monad
import           Data.Foldable
import           Data.Graph.Indices
import           Data.Graph.NodeContext
import           Data.Graph.Sequence.Class
import           Data.Graph.Type
import           Data.List.NonEmpty        (NonEmpty (..))
import qualified Data.List.NonEmpty        as NE
import           Data.Pair.Strict
import           Data.Semigroup.Foldable
import           Data.Vector               (Vector, fromList)


class HasScore g c where

    score :: g -> c


class HasLeafDecoration t t' | t' -> t where

    _leafDecoration :: Lens' t t'


class HasTreeDecoration n n' | n' -> n where

    _treeDecoration :: Lens' n n'


class HasFinalCacheDecoration c c' | c' -> c where

    _hasFinalCacheDecoration :: Lens' c c'


type FinalDecorationGraphM m c e n t =
  Graph
    m
    (FinalDecoration c)
    (FinalDecoration e)
    (FinalDecoration n)
    (FinalDecoration t)


type FinalDecorationGraph c e n t = FinalDecorationGraphM Identity c e n t


class (HasCharacterSequence' t (CharacterSequence t)) => HasGraphDecoration c e n t where

    decorate :: Graph Maybe c () n t -> FinalDecorationGraph c e n t


fromFoldable1 :: Foldable1 f => f a -> [a]
fromFoldable1 = NE.toList . toNonEmpty


rootsFromInds :: MonadPlus m => [Int] -> Vector (RootIndexData (m n) ())
rootsFromInds = fromList . fmap (\i -> rootIndexData mzero (Left $ childInfo LeafTag i ()))


initialBuild
  :: ( MonadPlus m
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
  :: forall c e n t charSeq f.
     ( HasGraphDecoration c e n t
--     , HasScore (FinalDecorationGraph c e n t) Double
--     , HasCharacterSequence t (CharacterSequence t)
--     , MetricSpace charSeq
--     , MedianSpace charSeq
     , Foldable1 f
    )
  => f t -> c -> FinalDecorationGraph c e n t
naiveWagnerBuild ts cache =
  case toNonEmpty ts of
      t:|[]  -> decorate @c @e @n @t
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
                              (Left $ childInfo LeafTag 0 ())
                          ]
                  , cachedData = cache
                  }

      x:|[y] -> decorate @c @e @n @t
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
                              (Right $ childInfo LeafTag 0 () :!: childInfo LeafTag 1 ())
                          ]
                  , cachedData = cache
                  }
      _:|(_:_:xs) ->
        let
          initTree :: FinalDecorationGraph c e n t
          initTree
              = decorate @c @e @n @t undefined

--                  [ ( mempty        , wipeNode True  x, IM.fromList [(1,mempty), (4,mempty)] )
--                  , ( IS.singleton 0, wipeNode True  x, IM.fromList [(2,mempty), (3,mempty)] )
--                  , ( IS.singleton 1, wipeNode False x, mempty )
--                  , ( IS.singleton 1, wipeNode False y, mempty )
--                  , ( IS.singleton 0, wipeNode False z, mempty )
--                  ]
        in
          foldl' (iterativeBuild @c @e @n @t @charSeq) initTree xs


iterativeBuild
  :: forall c e n t charSeq.
{-
     ( HasScore (FinalDecorationGraph c e n t) Double
     , HasGraphDecoration c e n t
     , HasCharacterSequence t (CharacterSequence t)
     , MetricSpace charSeq
     , MedianSpace charSeq
     )
  =>
-}
     FinalDecorationGraph c e n t
  -> t
  -> FinalDecorationGraph c e n t
iterativeBuild _currentGraph _tree = undefined
{-
  where
    newEdge :: (Int, ChildIndex)
    newEdge =
      let
        treeNodes = currentGraph ^. _treeReferences
        treeNodesInd
          :: Vector
               ( Int
               , TreeIndexData (Identity (FinalDecoration n)) (FinalDecoration e)
               )
        treeNodesInd = indexed treeNodes
        extract = second (view _right)
        minimumNode =
          extract . minimumBy (comparing $ view _right . snd) $
          parmap rpar (second edgeCosts)
          treeNodesInd
      in
        undefined
-}


{-
    edgeCosts :: TreeIndexData (Identity (FinalDecoration n)) (FinalDecoration e) -> ChildIndex :!: Double
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


        edgeCost :: FinalDecoration e -> Double
        edgeCost = undefined
-}
