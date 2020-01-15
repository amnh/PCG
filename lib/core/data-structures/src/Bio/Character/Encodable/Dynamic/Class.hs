-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Character.Encodable.Dynamic.Class
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Class for needed operations of coded sequences and characters
--
--
-----------------------------------------------------------------------------

{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UnboxedSums           #-}

module Bio.Character.Encodable.Dynamic.Class where

import Bio.Character.Encodable.Internal
import Bio.Character.Encodable.Stream
import Bio.Character.Exportable         (Subcomponent)
import Control.DeepSeq
import Data.Bits
import Data.Data
import Data.List.NonEmpty
import Data.MonoTraversable
import Data.Semigroup.Foldable
import GHC.Generics                     (Generic)


-- |
-- The four cases for an alignment type of a dynmaic character element
data  AlignmentContext
    = Gapping
    | Insertion
    | Deletion
    | Alignment
    deriving stock    (Data, Enum, Eq, Generic, Ord, Show)
    deriving anyclass (NFData)


-- type family Subcomponent median


-- |
-- Represents a character of variable length representing multiple encoded static characters.
--
-- > decodeMany alphabet . encodeMany alphabet == fmap toList . toList
--
class ( Bits (Subcomponent (Element s))
      , EncodableDynamicCharacterElement (Element s)
      , EncodableStream s
      , EncodedAmbiguityGroupContainer (Subcomponent (Element s))
      , Ord (Element s)
      , PossiblyMissingCharacter s
      ) => EncodableDynamicCharacter s where

    -- |
    -- Directly construct a dynamic character from the encoded elements.
    constructDynamic :: Foldable1 t => t (Element s) -> s

    destructDynamic :: s -> Maybe (NonEmpty (Element s))

--    encodeDynamic :: (Ord a, Foldable1 t, Foldable1 c, IsString a) => Alphabet a -> c (t a) -> s


class EncodableDynamicCharacterElement e where

    isGap, isInsert, isDelete, isAlign :: e -> Bool

    getContext    :: e -> AlignmentContext

    getMedian     :: (Subcomponent e -> Subcomponent e -> Subcomponent e) -> e -> Subcomponent e

    gapElement    :: Word -> e

    insertElement :: (Subcomponent e) -> e

    deleteElement :: (Subcomponent e) -> e

    alignElement  :: (Subcomponent e) -> (Subcomponent e) -> e

    swapContext   :: e -> e
