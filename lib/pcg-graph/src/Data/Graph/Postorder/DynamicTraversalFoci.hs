{-# LANGUAGE GADTs               #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts    #-}

module Data.Graph.Postorder.DynamicTraversalFoci where

import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HashMap
import Data.Graph.Postorder.Resolution
import Data.Graph.Type
import Data.Graph.Sequence.Class
import Data.Pair.Strict
import Control.Lens
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Control.Applicative
import Data.Graph.Indices
import Data.Graph.NodeContext
import qualified VectorBuilder.Builder as Builder
import qualified VectorBuilder.Vector as Builder
import VectorBuilder.Builder (Builder)
import Data.Key (foldMapWithKey)
import Data.Coerce



data EdgeReference nodeContext1 nodeContext2 = MkEdge
  { source :: !nodeContext1
  , target :: !nodeContext2
  }


--      (i)
--       |
--      (n)
--     /   \
--   (j)   (k)
data EdgeTreeMap edge edgeData = EdgeTreeMap
  { parentEdgeT     :: !(edge :!: edgeData)  -- (n) -> (i)
  , leftChildEdgeT  :: !(edge :!: edgeData)  -- (n) -> (j)
  , rightChildEdgeT :: !(edge :!: edgeData)  -- (n) -> (k)
  }

--      (i)
--       |
--      (n)
--     /   \
--   (j)   (k)
data EdgeNetworkMap edge edgeData = EdgeNetworkMap
  { parentEdge     :: !(edge :!: edgeData)  -- (n) -> (i)
  , leftChildEdge  :: !(edge :!: edgeData)  -- (n) -> (j)
  , rightChildEdge :: !(edge :!: edgeData)  -- (n) -> (k)
  }

type Edge = Int

assignOptimalDynamicCharacterRootEdges
  :: forall f block subBlock meta c e .
     ( BlockBin block
     , BlockBin subBlock
     , HasSequenceCost subBlock
     )
  => Lens' block subBlock
  -> Lens' (LeafBin block) (LeafBin subBlock)
  -> Lens' (CharacterMetadata block) (CharacterMetadata subBlock)
  -> Graph f c e (CharacterSequence block) (CharacterSequence (LeafBin block))
  -> ( HashMap Edge (ResolutionCache (CharacterSequence block))
     , GraphShape () () () ()

     )
assignOptimalDynamicCharacterRootEdges _subBlock _subLeaf _subMeta graph =
  case numberOfNodes graph of
    0 -> undefined
    1 -> undefined


getUnrootedEdges
  :: Graph f c e n t
  -> Vector EdgeIndex
getUnrootedEdges = liftA2 (<>) getNetworkEdges getTreeEdges


getRootEdges
  :: forall f c e n t . ()
  => Graph f c e n t
  -> Vector EdgeIndex
getRootEdges graph = Builder.build rootEdgesB
  where
    rootVec :: Vector (RootIndexData (f n) e)
    rootVec = view _rootReferences graph

    rootEdgesB :: Builder EdgeIndex
    rootEdgesB = foldMapWithKey buildEdges rootVec

    buildEdges :: Int -> RootIndexData (f n) e -> Builder EdgeIndex
    buildEdges ind treeData =
      let
        sourceTaggedIndex :: TaggedIndex
        sourceTaggedIndex = TaggedIndex ind RootTag

        childTaggedIndices :: Either TaggedIndex (TaggedIndex :!: TaggedIndex)
        childTaggedIndices = coerce $ view _childInds treeData

        oneChildHandler source target =
          Builder.singleton $ EdgeIndex {edgeSource = source, edgeTarget = target}

        twoChildHandler childInds =
          Builder.singleton
            EdgeIndex {edgeSource = view _left childInds, edgeTarget = view _right childInds}
      in
        either (oneChildHandler sourceTaggedIndex) twoChildHandler
          $ childTaggedIndices
  
  





  
