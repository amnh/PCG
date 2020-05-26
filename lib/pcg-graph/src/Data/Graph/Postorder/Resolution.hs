{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TypeApplications           #-}

module Data.Graph.Postorder.Resolution where

import           Control.DeepSeq
import           Control.Lens                      (Iso', Lens, Lens', iso, lens, over, set, view, (&), (.~))
import           Control.Monad                     (guard)
import           Data.Bits
import           Data.Functor.Apply
import           Data.Functor.Identity
import           Data.List.NonEmpty                (NonEmpty)
import qualified Data.List.NonEmpty                as NonEmpty
import           Data.Pair.Strict
import           Data.Proxy                        (Proxy (..))
import           Data.UnionSet
import           GHC.Generics
import           GHC.TypeLits

import           Data.Coerce
import           Data.Graph.Indices
import           Data.Graph.Node.Context
import           Data.Graph.NodeContext            (HasNodeData (..))
import qualified Data.Graph.NodeContext            as NodeContext
import           Data.Graph.Sequence.Class
import           Data.Graph.TopologyRepresentation
import           Data.Graph.Type
import           Data.Set                          (Set)
import qualified Data.Set                          as Set
import           Data.Vector                       (Vector)
import qualified Data.Vector                       as Vector
import qualified Data.Vector                       as Vector

-- |
-- The metadata of a subtree resolution.
data  ResolutionMetadata
    = ResolutionMetadata
    { totalSubtreeCost       :: {-# UNPACK #-} !Double
    , leafSetRepresentation  :: {-# UNPACK #-} !UnionSet
    , topologyRepresentation :: {-# UNPACK #-} !NetworkTopology
    , subTreeEdgeSet         :: {-# UNPACK #-} !(Set (EdgeIndex))
    , subTreeHash            :: {-# UNPACK #-} !Int
    }
    deriving stock    (Eq, Ord, Generic)
    deriving anyclass (NFData)

leafResolutionMetadata :: Int -> CharacterSequence block -> ResolutionMetadata
leafResolutionMetadata untaggedInd characterSequence = error "to do"

combineResolutionMetadata
  :: Double
  -> ResolutionMetadata
  -> ResolutionMetadata
  -> ResolutionMetadata
combineResolutionMetadata
  newCost
  (ResolutionMetadata total1 leafSet1 topology1 edge1 hash1)
  (ResolutionMetadata total2 leafSet2 topology2 edge2 hash2) =
  ResolutionMetadata
  -- Note: This adds the subtree resolution metadata but doesn't add the
  -- node metadata which must be done during the post order so we have
  -- access to the relevant information.
    (newCost + total1 + total2)
    (leafSet1  <> leafSet2)
    (topology1 <> topology2)
    (edge1     <> edge2)
    -- TODO : once we decide how to hash trees we should re-visit this
    -- to give a better account. One possiblity is to have a newtype
    -- with a monoid instance that uses bits to keep track of the
    -- tree we have.
    (hash1 + hash2)


-- |
-- Adds an edge reference to an existing subtree resolution.
-- This should not be called by itself but instead invoked by
-- `updateResolutionMetadataFromNetwork` or `UpdateResolutionMetadataFromTree`
addEdgeToSet :: EdgeIndex -> Resolution s -> Resolution s
addEdgeToSet e r =
  over (_resolutionMetadata . _subTreeEdgeSet) (<> Set.singleton e) r


-- |
-- This is the function we call to do all resolution metadata updates necessary
-- from a network node during a post order traversal.
updateResMetaSingle
  :: TaggedIndex -> TaggedIndex -> Resolution s -> Resolution s
updateResMetaSingle currNodeInd childInd
  = addEdgeToSet (EdgeIndex {edgeSource = currNodeInd, edgeTarget = childInd})

-- |
-- This is the function we call to do all resolution metadata updates necessary
-- from a network node during a post order traversal.
updateResMetaBinary
  :: TaggedIndex -> TaggedIndex -> TaggedIndex -> Resolution s -> Resolution s
updateResMetaBinary currNodeInd leftChildInd rightChildInd res
  = undefined

updateResCacheMetaBinary
  :: TaggedIndex -> TaggedIndex -> TaggedIndex -> ResolutionCache cs -> ResolutionCache cs
updateResCacheMetaBinary currNodeInd leftChildInd rightChildInd =
  mapResolution (updateResMetaBinary currNodeInd leftChildInd rightChildInd)



updateResCacheMetaSingle
  :: TaggedIndex -> TaggedIndex -> ResolutionCache cs -> ResolutionCache cs
updateResCacheMetaSingle currNodeInd childInd =
  mapResolution (updateResMetaSingle currNodeInd childInd)






-- |
-- Assert that the two leafsets in a resolution do not overlap.
-- This is used when we propagate resolutions upwards during a postorder.
-- This rules out the possibility of including _both_ network edges within
-- a single chosen resolution.
disjointResolutions
  :: ( HasLeafSetRepresentation res1 UnionSet
     , HasLeafSetRepresentation res2 UnionSet)
  => res1 -> res2 -> Bool
disjointResolutions x y
  = popCount ((view _leafSetRepresentation x) .&. (view _leafSetRepresentation y)) == 0



data Resolution cs = Resolution
  { characterSequence  :: !cs
  , resolutionMetadata :: !ResolutionMetadata
  }
  deriving stock    (Functor, Foldable, Traversable, Generic)
  deriving anyclass (NFData)


leafResolution
  :: (BlockBin block)
  => Int
  -> CharacterSequence (LeafBin block)
  -> MetadataSequence block meta
  -> Resolution (CharacterSequence block)
leafResolution untaggedInd leafSequence meta = Resolution{..}
  where
    characterSequence  = characterLeafInitialise meta leafSequence
    resolutionMetadata = leafResolutionMetadata untaggedInd characterSequence


applySoftwireResolutions
  :: forall block . ()
  => TaggedIndex
  -> ChildContext     -- ^ Possible subtree resolution information
       (TaggedIndex, (ResolutionCache (CharacterSequence block)))
  -> ResolutionCache  -- ^ Potential subtree contexts
       (PostorderContext (CharacterSequence (LeafBin block)) (CharacterSequence block))
applySoftwireResolutions currNodeIndex =
    \case
      OneChild (childIndexType, childCache) ->
        let
          updatedResCache
            = updateResCacheMetaSingle currNodeIndex childIndexType $
                childCache
        in
          PostNetworkContext <$> updatedResCache

      TwoChildren leftCtxt rightCtxt ->
        let
          pairingLogic (leftIndex, leftCache) (rightIndex, rightCache) =
            let
           -- This updates the resolution metadata to account for taking both child
           -- resolutions
              updateMetaBinary   =  updateResCacheMetaBinary currNodeIndex leftIndex rightIndex
           -- This function takes a tagged index argument and updates the metadata
           -- from the resolutions in that argument
              updateMetaSingle   =  updateResCacheMetaSingle currNodeIndex
           -- The left and right node types
              leftIndexType      = view _indexType leftIndex
              rightIndexType     = view _indexType rightIndex
           -- These are the resolutions for which include both child nodes in the display tree
              pairedResolutions
                = updateMetaBinary $ liftF2 PostBinaryContext leftCache rightCache
           -- The resolutions where we do not include the right child edge
              lhsNetResolutions
                = updateMetaSingle leftIndex  . fmap PostNetworkContext $ leftCache
           -- The resolutions where we do not include the right child edge
              rhsNetResolutions
                = updateMetaSingle rightIndex . fmap PostNetworkContext $ rightCache
           -- This is all possible combinations of resolutions
              allResolutions = pairedResolutions <> lhsNetResolutions <> rhsNetResolutions
            in
            case (leftIndexType, rightIndexType) of
           -- if both children are network nodes then include all possible resoluitions
              (NetworkTag, NetworkTag) -> allResolutions
           -- if only the left node is a network node then we include those edges
           -- resolutions which do not include this edge in the display tree
              (NetworkTag, _         ) -> pairedResolutions <> rhsNetResolutions
           -- mutatis mutandis
              (_         , NetworkTag) -> pairedResolutions <> lhsNetResolutions
           -- If neither child is a network node then take all those network
           -- nodes which are compatible from each.
              (_         , _         ) -> pairedResolutions
        in
          pairingLogic leftCtxt rightCtxt


virtualParentResolution
  :: forall block subBlock dynChar meta .
     ( BlockBin block
     , BlockBin subBlock
     , HasBlockCost block
     , DynCharacterSubBlock subBlock dynChar
     )
  => Lens' block subBlock
  -> Lens' (CharacterMetadata block) (CharacterMetadata subBlock)
  -> MetadataSequence block meta
  -> (TaggedIndex, ResolutionCache (CharacterSequence block))
  -> (TaggedIndex, ResolutionCache (CharacterSequence block))
  -> ResolutionCache (CharacterSequence block)
virtualParentResolution
    _subBlock
    _subMeta
    blockMeta
    (leftChildIndex, leftResolutions)
    (rightChildIndex, rightResolutions) =
  let
    resolutionContext
      :: ResolutionCache (PostorderContext
                            (CharacterSequence (LeafBin block))
                            (CharacterSequence block))

    virtualRootIndex :: TaggedIndex
    virtualRootIndex = TaggedIndex 0 RootTag

    resolutionContext
      = applySoftwireResolutions
          virtualRootIndex $
          TwoChildren
            (leftChildIndex, leftResolutions )
            (rightChildIndex, rightResolutions)

    updatedBlockResolutions
      :: ResolutionCache (CharacterSequence block)
    updatedBlockResolutions =
      let
        subBlockUpdate =
          generateSubBlockLocalResolutions
            _subBlock
            _subMeta
            blockMeta
      in
          coerce                 -- Convert back to abstract type
        . fmap subBlockUpdate    -- Update each subBlock accordingly
        . view _resolutionCache  -- Convert to nonempty list of resolutions
        $ resolutionContext
  in
    updatedBlockResolutions

generateLocalResolutions
  :: ( BlockBin block
     , HasBlockCost block
     )
  => MetadataSequence block meta
  -> Resolution
       (PostorderContext
         (CharacterSequence (LeafBin block))
         (CharacterSequence block)
       )
  -> Resolution (CharacterSequence block)
generateLocalResolutions meta childResolutionContext =
  case (view _characterSequence childResolutionContext) of
        LeafContext leafCharSequence ->
          let
            newCharacterSequence = characterLeafInitialise meta leafCharSequence
            newTotalCost = sequenceCost meta newCharacterSequence
          in
            childResolutionContext
              & _characterSequence .~ newCharacterSequence
              & _totalSubtreeCost  .~ newTotalCost

        PostNetworkContext netChildCharSequence ->
          childResolutionContext & _characterSequence .~ netChildCharSequence
          -- Want to propogate what is stored in the network child resolution
          -- to the parent.

        PostBinaryContext { leftChild  = leftCharSequence, rightChild = rightCharSequence} ->
          let
            newCharacterSequence
              = characterBinaryPostorder meta leftCharSequence rightCharSequence
            newTotalCost
              = sequenceCost meta newCharacterSequence
          in
            childResolutionContext
              & _characterSequence .~ newCharacterSequence
              & _totalSubtreeCost  .~ newTotalCost


generateSubBlockLocalResolutions
  :: forall block subBlock meta .
     ( BlockBin block
     , BlockBin subBlock
     , HasBlockCost block
     )
  => Lens' block subBlock
  -> Lens' (CharacterMetadata block) (CharacterMetadata subBlock)
  -> MetadataSequence block meta
  -> Resolution
       (PostorderContext
         (CharacterSequence (LeafBin block))
         (CharacterSequence block)
       )
  -> Resolution (CharacterSequence block)
generateSubBlockLocalResolutions _subBlock _subMeta meta childResolutionContext =
  case (view _characterSequence childResolutionContext) of
    LeafContext leafCharSequence ->
      let
        newCharacterSequence = characterLeafInitialise meta leafCharSequence
        newTotalCost = sequenceCost meta newCharacterSequence
      in
        childResolutionContext
          & _characterSequence .~ newCharacterSequence
          & _totalSubtreeCost  .~ newTotalCost
    PostNetworkContext netChildCharSequence ->
      childResolutionContext & _characterSequence .~ netChildCharSequence
      -- Want to propogate what is stored in the network child resolution
      -- to the parent.

      -- So far this is the same as generate local resolution, in the case
      -- where we have resolutions from each of our children we then update
      -- only the subblock that are leneses pick out.
    PostBinaryContext
      { leftChild  = leftCharSequence, rightChild = rightCharSequence} ->
      let
        -- update blockwise the subblocks in each bin
        updateSubBlock
          :: CharacterSequence subBlock
          -> CharacterSequence block
          -> CharacterSequence block
        updateSubBlock = coerce $ Vector.zipWith (set _subBlock)

        -- extract out the metadata sequence of the subblock
        subMeta :: MetadataSequence subBlock meta
        subMeta
          = coerce                                    -- convert back to metadata sequence
          . fmap (over _binMetadata (view _subMeta))  -- extract subBlock metadata
          . view _blockSequence                       -- convert to a vector of metadata blocks
          $ meta

        -- Extract the subblock character sequences
        leftSubBlockSequence  :: CharacterSequence subBlock
        rightSubBlockSequence :: CharacterSequence subBlock
        leftSubBlockSequence  = view _subBlock <$> leftCharSequence
        rightSubBlockSequence = view _subBlock <$> rightCharSequence

        -- This is the updated character sequence on the subblocks
        subBlockCharacterSequence :: CharacterSequence subBlock
        subBlockCharacterSequence
          = characterBinaryPostorder subMeta leftSubBlockSequence rightSubBlockSequence

        -- Put the updated subblock character sequences back into the original
        newCharacterSequence :: CharacterSequence block
        newCharacterSequence = updateSubBlock subBlockCharacterSequence leftCharSequence

        -- Finally recompute the cost for the new character sequence
        newTotalCost
          = sequenceCost meta newCharacterSequence
      in
        childResolutionContext
          & _characterSequence .~ newCharacterSequence
          & _totalSubtreeCost  .~ newTotalCost


getResolutionCache
  :: MetadataSequence block meta
  -> TaggedIndex
  -> Graph
       (ResolutionCacheM Identity)
       cache
       e
       (CharacterSequence block)
       (CharacterSequence (LeafBin block))
  -> ResolutionCache (CharacterSequence block)
getResolutionCache meta taggedIndex graph =
  case view _indexType taggedIndex of
    LeafTag ->
      let
        untaggedInd = view _untaggedIndex taggedIndex
        node = indexLeaf graph untaggedInd
        nodeInfo = view _nodeData node
      in
        makeLeafResolution untaggedInd nodeInfo
    RootTag ->
      let
        node = indexRoot graph (view _untaggedIndex taggedIndex)
        nodeInfo = view _nodeData node
      in
        nodeInfo
    NetworkTag ->
      let
        node = indexNetwork graph (view _untaggedIndex taggedIndex)
        nodeInfo = view _nodeData node
      in
        nodeInfo
    TreeTag ->
      let
        node = indexTree graph (view _untaggedIndex taggedIndex)
        nodeInfo = view _nodeData node
      in
        nodeInfo
  where
    makeLeafResolution
      :: Int
      -> CharacterSequence (LeafBin block)
      -> ResolutionCache (CharacterSequence block)
    makeLeafResolution untaggedInd leafCharSeq = undefined


type ResolutionCache cs = ResolutionCacheM Identity cs
type CharacterResolutionCache block = ResolutionCache (CharacterSequence block)


_resolutionCache :: Iso' (ResolutionCache cs) (NonEmpty (Resolution cs))
{-# INLINE _resolutionCache #-}
_resolutionCache = iso coerce coerce


singleton :: Resolution cs -> ResolutionCache cs
{-# INLINE singleton #-}
singleton = ResolutionCacheM . Identity . pure


newtype ResolutionCacheM m cs
  = ResolutionCacheM {runResolutionCacheM :: m (NonEmpty (Resolution cs))}

-- to do: We can't partially apply a type synonym so we should figure out a better story with
-- resolutionCache, probably could make it a newtype that takes the instances from ResolutionCacheM or just remove ResolutionCacheM

mapResolution
  :: forall m cs cs' . (Functor m)
  => (Resolution cs -> Resolution cs')
  -> (ResolutionCacheM m cs -> ResolutionCacheM m cs')
{-# INLINE mapResolution #-}
mapResolution resFn =
    ResolutionCacheM
  . fmap (fmap resFn)
  . runResolutionCacheM


filterResolutionM
  :: forall m cs cs' . (Functor m)
  => (Resolution cs -> Bool)
  -> (ResolutionCacheM m cs -> m [(Resolution cs)])
filterResolutionM p =
    fmap (NonEmpty.filter p)
  . runResolutionCacheM

filterResolution
  :: (Resolution cs -> Bool)
  -> ResolutionCache cs
  -> [Resolution cs]
filterResolution p = runIdentity . filterResolutionM p
  




instance Apply m => Semigroup (ResolutionCacheM m cs) where
  ls <> rs =
    ResolutionCacheM $
      liftF2 (<>)
      (runResolutionCacheM ls)
      (runResolutionCacheM rs)




newtype SelectMonad (n :: Nat) m a = SelectMonad {runSelectMonad :: m a}
  deriving newtype (Functor, Applicative, Monad)


class Monad m => MonadSelect m where
  select :: NonEmpty a -> m [a]


instance (KnownNat n, MonadSelect m) => MonadSelect (SelectMonad n m) where
  select ne = take (fromIntegral $ natVal (Proxy :: Proxy n)) <$> select ne


instance MonadSelect Identity where
  select = Identity .  NonEmpty.toList


instance (Functor m) => Functor (ResolutionCacheM m) where
  fmap f (ResolutionCacheM resolutionM) =
    ResolutionCacheM (fmap (fmap (fmap f)) resolutionM)

instance (Monad m) => Apply (ResolutionCacheM m) where
  liftF2 f (ResolutionCacheM leftResolutionsM) (ResolutionCacheM rightResolutionsM) =
    ResolutionCacheM $
      do
        leftResolutions  <- leftResolutionsM
        rightResolutions <- rightResolutionsM
        pure $ NonEmpty.fromList $
          do
            leftResolution@(Resolution  leftCharacterSequence leftResolutionMetadata)
              <- NonEmpty.toList leftResolutions
            rightResolution@(Resolution rightCharacterSequence rightResolutionMetadata)
              <- NonEmpty.toList rightResolutions
            guard $ disjointResolutions leftResolution rightResolution
            let charSeqCost = error "to do : charSeqCost"
            let newMetadata
                  = combineResolutionMetadata
                    charSeqCost
                    leftResolutionMetadata
                    rightResolutionMetadata
            let newCharacterSequence = f leftCharacterSequence rightCharacterSequence
            pure $ Resolution newCharacterSequence newMetadata

instance (Show (m a), Show a) => Show (ResolutionCacheM m a) where
  show = error "to do: show ResolutionCahceM"


-- |
-- A 'Lens' for the 'totalSubtreeCost' field in 'ResolutionMetadata'
{-# SPECIALISE _totalSubtreeCost :: Lens' ResolutionMetadata Double #-}
{-# SPECIALISE _totalSubtreeCost :: Lens' (Resolution s) Double #-}
class HasTotalSubtreeCost s a | s -> a where

    _totalSubtreeCost :: Lens' s a

instance HasTotalSubtreeCost ResolutionMetadata Double where

    {-# INLINE _totalSubtreeCost #-}
    _totalSubtreeCost = lens totalSubtreeCost (\r t -> r {totalSubtreeCost = t})


instance HasTotalSubtreeCost (Resolution s) Double where

    {-# INLINE _totalSubtreeCost #-}
    _totalSubtreeCost = _resolutionMetadata . _totalSubtreeCost

{-
-- |
-- A 'Lens' for the 'localSequencecost' field in 'ResolutionMetadata'
{-# SPECIALISE _localSequenceCost :: Lens' ResolutionMetadata Double #-}
{-# SPECIALISE _localSequenceCost :: Lens' (Resolution s) Double #-}
class HasLocalSequenceCost s a | s -> a where

    _localSequenceCost :: Lens' s a


instance HasLocalSequenceCost ResolutionMetadata Double where

    {-# INLINE _localSequenceCost #-}
    _localSequenceCost = lens localSequenceCost (\r t -> r {localSequenceCost = t})


instance HasLocalSequenceCost (Resolution s) Double where

    {-# INLINE _localSequenceCost #-}
    _localSequenceCost = _resolutionMetadata . _localSequenceCost
-}

-- |
-- A 'Lens' for the 'LeafSetRepresentation' field in 'ResolutionMetadata'
{-# SPECIALISE _leafSetRepresentation :: Lens' ResolutionMetadata UnionSet #-}
{-# SPECIALISE _leafSetRepresentation :: Lens' (Resolution s) UnionSet #-}
class HasLeafSetRepresentation s a | s -> a where

    _leafSetRepresentation :: Lens' s a


instance HasLeafSetRepresentation ResolutionMetadata UnionSet where

    {-# INLINE _leafSetRepresentation #-}
    _leafSetRepresentation = lens leafSetRepresentation (\r l -> r {leafSetRepresentation = l})


instance HasLeafSetRepresentation (Resolution s) UnionSet where

    {-# INLINE _leafSetRepresentation #-}
    _leafSetRepresentation = _resolutionMetadata . _leafSetRepresentation



-- |
-- A 'Lens' for the 'topologyRepresentation' field in 'ResolutionMetadata'
{-# SPECIALISE _topologyRepresentation :: Lens' ResolutionMetadata (TopologyRepresentation (Int :!: Int))  #-}
{-# SPECIALISE _topologyRepresentation :: Lens' (Resolution s) (TopologyRepresentation (Int :!: Int))  #-}
class HasTopologyRepresentation s a | s -> a where

    _topologyRepresentation :: Lens' s a


instance HasTopologyRepresentation ResolutionMetadata (NetworkTopology) where

    {-# INLINE _topologyRepresentation #-}
    _topologyRepresentation = lens topologyRepresentation (\r t -> r {topologyRepresentation = t})


instance HasTopologyRepresentation (Resolution s) (NetworkTopology) where

    {-# INLINE _topologyRepresentation #-}
    _topologyRepresentation = _resolutionMetadata . _topologyRepresentation

-- |
-- A 'Lens' for the 'subTreeHash' field in 'ResolutionMetadata'
{-# SPECIALISE _subTreeHash :: Lens' ResolutionMetadata Int #-}
{-# SPECIALISE _subTreeHash :: Lens' (Resolution s)     Int #-}
class HasSubTreeHash s a | s -> a where
  _subTreeHash :: Lens' s a


instance HasSubTreeHash ResolutionMetadata Int where

    {-# INLINE _subTreeHash #-}
    _subTreeHash = lens subTreeHash (\r t -> r {subTreeHash = t})


instance HasSubTreeHash (Resolution s) Int where

    {-# INLINE _subTreeHash #-}
    _subTreeHash = _resolutionMetadata . _subTreeHash



{-# SPECIALISE _characterSequence :: Lens (Resolution s) (Resolution s') s s' #-}
instance HasCharacterSequence (Resolution cs) (Resolution cs') cs cs' where

    {-# INLINE _characterSequence #-}
    _characterSequence = lens characterSequence (\r c -> r {characterSequence = c})


-- |
-- A 'Lens' for the 'resolutionMetadata' field in 'Resolution'
{-# SPECIALISE  _resolutionMetadata :: Lens' (Resolution s) ResolutionMetadata  #-}
class HasResolutionMetadata s a | s -> a where

    _resolutionMetadata :: Lens' s a


instance HasResolutionMetadata (Resolution s) ResolutionMetadata where

    {-# INLINE _resolutionMetadata #-}
    _resolutionMetadata = lens resolutionMetadata (\r m -> r {resolutionMetadata = m})



-- |
-- A 'Lens' for the 'subtreeSet' field in 'ResolutionMetadata'
{-# SPECIALISE _subTreeEdgeSet:: Lens' ResolutionMetadata (Set EdgeIndex) #-}
{-# SPECIALISE _subTreeEdgeSet :: Lens' (Resolution s) (Set EdgeIndex) #-}
class HasSubtreeSet s a | s -> a where

    _subTreeEdgeSet :: Lens' s a


instance HasSubtreeSet ResolutionMetadata (Set EdgeIndex) where

    {-# INLINE _subTreeEdgeSet #-}
    _subTreeEdgeSet = lens subTreeEdgeSet (\r s -> r {subTreeEdgeSet = s})


instance HasSubtreeSet (Resolution s) (Set EdgeIndex) where

    {-# INLINE _subTreeEdgeSet #-}
    _subTreeEdgeSet = _resolutionMetadata . _subTreeEdgeSet
