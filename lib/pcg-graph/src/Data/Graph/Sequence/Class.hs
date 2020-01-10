{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE FlexibleContexts       #-}

module Data.Graph.Sequence.Class where

import Control.Lens
import Data.Kind

class MetricSpace charSeq where
  dist :: charSeq -> charSeq -> Double

class MedianSpace charSeq where
  median :: charSeq -> charSeq -> charSeq

-- |
-- A 'Lens' for the 'characterSequence' field
class HasCharacterSequence s t a b | s -> a, t -> b, s b -> t, t a -> s where

    _characterSequence :: Lens s t a b

-- |
-- A 'Lens' for the 'characterSequence' field
class HasCharacterSequence' s a | s -> a where

    _characterSequence' :: Lens' s a


class AssociatedCharacterMetadata charType charMeta | charType -> charMeta where

class MetadataSequence meta where


-- |
-- A 'Lens' for the 'characterCost' field.
class HasCharacterCost s a | s -> a where

    {-# MINIMAL characterCost #-}
    characterCost :: Lens' s a

-- |
-- CharacterBlocks satisfying this constraint have a calculable cost.
type HasBlockCost u v w x y z =
    ( HasCharacterCost u Double
    , HasCharacterCost v Word
    , HasCharacterCost w Word
    , HasCharacterCost x Word
    , HasCharacterCost y Word
    , HasCharacterCost z Word
    )

class HasSequenceCost charSeq where
  sequenceCost
    :: (HasBlockCost u v w x y z, MetadataSequence meta)
    => meta
    -> charSeq u v w x y z
    -> Double
  



type family FinalDecoration   a :: Type
type family CharacterSequence a :: Type


class HexZippableMeta (charSeq :: Type -> Type -> Type -> Type -> Type -> Type -> Type) where
  hexZipMeta
     :: ( AssociatedCharacterMetadata continuousMeta   u
        , AssociatedCharacterMetadata discreteMeta     v
        , AssociatedCharacterMetadata discreteMeta'    w
        , AssociatedCharacterMetadata discreteWithTCM  x
        , AssociatedCharacterMetadata discreteWithTCM' y
        , AssociatedCharacterMetadata dynamicMeta      z
        , MetadataSequence            meta
        )
     => (continuousMeta   -> u -> u')
     -> (discreteMeta     -> v -> v')
     -> (discreteMeta'    -> w -> w')
     -> (discreteWithTCM  -> x -> x')
     -> (discreteWithTCM' -> y -> y')
     -> (dynamicMeta      -> z -> z')
     -> meta
     -> charSeq u   v   w   x   y   z
     -> charSeq u'  v'  w'  x'  y'  z'



  hexZipMeta2
   :: ( AssociatedCharacterMetadata continuousMeta   u
      , AssociatedCharacterMetadata discreteMeta     v
      , AssociatedCharacterMetadata discreteMeta'    w
      , AssociatedCharacterMetadata discreteWithTCM  x
      , AssociatedCharacterMetadata discreteWithTCM' y
      , AssociatedCharacterMetadata dynamicMeta      z
      , MetadataSequence            meta
      )
   => (continuousMeta   -> u -> u  -> u')
   -> (discreteMeta     -> v -> v  -> v')
   -> (discreteMeta'    -> w -> w  -> w')
   -> (discreteWithTCM  -> x -> x  -> x')
   -> (discreteWithTCM' -> y -> y  -> z')
   -> (dynamicMeta      -> z -> z  -> z')
   -> meta
   -> charSeq u   v   w   x   y   z
   -> charSeq u   v   w   x   y   z
   -> charSeq u'  v'  w'  x'  y'  z'
