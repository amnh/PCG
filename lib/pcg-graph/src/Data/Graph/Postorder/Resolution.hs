{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeOperators              #-}

module Data.Graph.Postorder.Resolution where

import           Control.DeepSeq
import           Control.Lens                      (Lens, Lens', lens, view, (&), (.~))
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

import           Data.Graph.TopologyRepresentation
import           Data.Graph.Sequence.Class
import           Data.Graph.Node.Context

-- |
-- The metadata of a subtree resolution.
data  ResolutionMetadata
    = ResolutionMetadata
    { totalSubtreeCost       :: {-# UNPACK #-} !Double
    , leafSetRepresentation  :: {-# UNPACK #-} !UnionSet
    , topologyRepresentation :: {-# UNPACK #-} !(TopologyRepresentation (Int :!: Int))
    , subTreeHash            :: {-# UNPACK #-} !Int
    }
    deriving stock    (Eq, Ord, Generic)
    deriving anyclass (NFData)

combineResolutionMetadata
  :: Double
  -> ResolutionMetadata
  -> ResolutionMetadata
  -> ResolutionMetadata
combineResolutionMetadata
  newCost
  (ResolutionMetadata total1 leafSet1 topology1 hash1)
  (ResolutionMetadata total2 leafSet2 topology2 hash2) =
  ResolutionMetadata
    (newCost + total1 + total2)
    (leafSet1  <> leafSet2)
    (topology1 <> topology2)
    -- TODO : once we decide how to hash trees we should re-visit this
    -- to give a better account. One possiblity is to have a newtype
    -- with a monoid instance that uses bits to keep track of the
    -- tree we have.
    (hash1 + hash2)



-- |
-- Assert that the two leafsets in a resolution do not overlap
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

--virtualParentResolution
--  :: ResolutionCache cs -> ResolutionCache cs

generateLocalResolutions
  :: ( AssociatedCharacterMetadata continuousMeta   u
     , AssociatedCharacterMetadata continuousMeta   u'
     , AssociatedCharacterMetadata discreteMeta     v
     , AssociatedCharacterMetadata discreteMeta'    w
     , AssociatedCharacterMetadata discreteWithTCM  x
     , AssociatedCharacterMetadata discreteWithTCM' y
     , AssociatedCharacterMetadata dynamicMeta      z
     , MetadataSequence            meta
     , HexZippableMeta      charSeq
     )
  => (continuousMeta   -> PostorderContext u u' -> u')
  -> (discreteMeta     -> PostorderContext v v' -> v')
  -> (discreteMeta'    -> PostorderContext w w' -> w')
  -> (discreteWithTCM  -> PostorderContext x x' -> x')
  -> (discreteWithTCM' -> PostorderContext y y' -> z')
  -> (dynamicMeta      -> PostorderContext z z' -> z')
  -> meta
  -> Resolution
       (PostorderContext
         (charSeq u  v  w  x  y  z )
         (charSeq u' v' w' x' y' z')
       )
  -> Resolution (charSeq u' v' w' x' y' z')
generateLocalResolutions f1 f2 f3 f4 f5 f6 meta childResolutionContext =
  let
    (f1Leaf, f1Bin) = (leafFunction <$> f1 , postBinaryFunction <$> f1)
    (f2Leaf, f2Bin) = (leafFunction <$> f2 , postBinaryFunction <$> f2)
    (f3Leaf, f3Bin) = (leafFunction <$> f3 , postBinaryFunction <$> f3)
    (f4Leaf, f4Bin) = (leafFunction <$> f4 , postBinaryFunction <$> f4)
    (f5Leaf, f5Bin) = (leafFunction <$> f5 , postBinaryFunction <$> f5)
    (f6Leaf, f6Bin) = (leafFunction <$> f6 , postBinaryFunction <$> f6)
  in  case (view _characterSequence childResolutionContext) of
        LeafContext leafCharSequence ->
          let newCharacterSequence
                = hexZipMeta
                    f1Leaf
                    f2Leaf
                    f3Leaf
                    f4Leaf
                    f5Leaf
                    f6Leaf
                    meta
                    leafCharSequence
              newTotalCost = sequenceCost meta newCharacterSequence
          in
            childResolutionContext
              & _characterSequence .~ newCharacterSequence
              & _totalSubtreeCost  .~ newTotalCost

        PostNetworkContext netChildCharSequence ->
          childResolutionContext & _characterSequence .~ netChildCharSequence
          -- Want to propogate what is stored in the network child resolution
          -- to the parent.

        PostBinaryContext
          { leftChild  = leftCharSequence
          , rightChild = rightCharSequence
          } ->
          let
            totalChildCost = view _totalSubtreeCost childResolutionContext
            newCharacterSequence
                = hexZipMeta2
                    f1Bin
                    f2Bin
                    f3Bin
                    f4Bin
                    f5Bin
                    f6Bin
                    meta
                    leftCharSequence
                    rightCharSequence
                    
            newTotalCost      = sequenceCost meta newCharacterSequence
            newLocalCost      = newTotalCost - totalChildCost
              in
            resolutionContext
              & _characterSequence .~ newCharacterSequence
              & _totalSubtreeCost  .~ newTotalCost
              & _localSequenceCost .~ newLocalCost

type ResolutionCache cs = ResolutionCacheM Identity cs


newtype ResolutionCacheM m cs
  = ResolutionCacheM {runResolutionCacheM :: m (NonEmpty (Resolution cs))}


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


instance HasTopologyRepresentation ResolutionMetadata (TopologyRepresentation (Int :!: Int)) where

    {-# INLINE _topologyRepresentation #-}
    _topologyRepresentation = lens topologyRepresentation (\r t -> r {topologyRepresentation = t})


instance HasTopologyRepresentation (Resolution s) (TopologyRepresentation (Int :!: Int)) where

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
