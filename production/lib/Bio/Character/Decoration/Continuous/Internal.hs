-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Character.Decoration.Continuous.Internal
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts, FlexibleInstances, FunctionalDependencies, MultiParamTypeClasses #-}

module Bio.Character.Decoration.Continuous.Internal where


import Bio.Character.Decoration.Continuous.Class
import Bio.Character.Decoration.Shared
import Bio.Character.Encodable
import Bio.Metadata.Continuous
import Bio.Metadata.CharacterName
import Control.Lens
import Data.Word


-- |
-- An abstract initial dynamic character decoration with a polymorphic character
-- type.
data ContinuousDecorationInitial c
   = ContinuousDecorationInitial
   { continuousDecorationInitialCharacter :: c
   , continuousMetadaField                :: ContinuousCharacterMetadataDec
   , continuousIsLeaf                     :: Bool
   , continuousMinCost                    :: Double
   , continuousPreliminaryMedian          :: c
   , continuousInterval                   :: (Word32, Word32)
   , continuousChildPrelims               :: ((Word32, Word32), (Word32, Word32))
   }


-- |
-- A newtype wrapper for a possibly missing continuous.
newtype ContinuousChar = CC (Maybe Double)
  deriving (Eq,Ord)


-- | (✔)
instance Show ContinuousChar where

    show (CC  Nothing) = "?"
    show (CC (Just x)) = show x


-- | (✔)
instance Show c => Show (ContinuousDecorationInitial c) where

    show = show . (^. continuousCharacter)


-- | (✔)
instance PossiblyMissingCharacter c => PossiblyMissingCharacter (ContinuousDecorationInitial c) where

    isMissing = isMissing . (^. continuousCharacter)

    toMissing x = x & continuousCharacter %~ toMissing


-- | (✔)
instance PossiblyMissingCharacter ContinuousChar where

    {-# INLINE toMissing #-}
    toMissing = const $ CC Nothing

    {-# INLINE isMissing #-}
    isMissing (CC Nothing) = True
    isMissing _            = False


-- | (✔)
instance ContinuousCharacter ContinuousChar where

    toContinuousCharacter = CC . fmap (fromRational . toRational)


-- | (✔)
instance HasCharacterName (ContinuousDecorationInitial c) CharacterName where

    characterName = lens getter setter
      where
         getter e   = continuousMetadaField e ^. characterName
         setter e x = e { continuousMetadaField = continuousMetadaField e &  characterName .~ x }


-- | (✔)
instance HasCharacterWeight (ContinuousDecorationInitial c) Double where

    characterWeight = lens getter setter
      where
         getter e   = continuousMetadaField e ^. characterWeight
         setter e x = e { continuousMetadaField = continuousMetadaField e &  characterWeight .~ x }


-- | (✔)
instance HasContinuousCharacter (ContinuousDecorationInitial c) c where

    continuousCharacter = lens continuousDecorationInitialCharacter $ \e x -> e { continuousDecorationInitialCharacter = x }


-- | (✔)
instance HasIsLeaf (ContinuousDecorationInitial f) Bool where

    isLeaf = lens continuousIsLeaf (\e x -> e { continuousIsLeaf = x })


-- | (✔)
instance GeneralCharacterMetadata (ContinuousDecorationInitial d) where


-- | (✔)
instance ContinuousCharacter c => ContinuousDecoration (ContinuousDecorationInitial c) c where


-- | A smart constructor for a continuous character.
continuousDecorationInitial :: CharacterName -> (x -> c) -> x -> ContinuousDecorationInitial c
continuousDecorationInitial name f v =
    ContinuousDecorationInitial
    { continuousDecorationInitialCharacter = f v
    , continuousMetadaField                             = continuousMetadata name 1
    }
