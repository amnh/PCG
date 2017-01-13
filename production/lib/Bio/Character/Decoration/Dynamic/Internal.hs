-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Character.Decoration.Dynamic.Internal
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, TypeFamilies #-}

{-# LANGUAGE UndecidableInstances #-}

module Bio.Character.Decoration.Dynamic.Internal where

import Bio.Character.Decoration.Dynamic.Class
import Bio.Character.Encodable
import Bio.Metadata.CharacterName
import Bio.Metadata.Discrete
import Control.Lens
import Data.Alphabet
import Data.MonoTraversable


-- |
-- An abstract initial dynamic character decoration with a polymorphic character
-- type.
data DynamicDecorationInitial d
   = DynamicDecorationInitial
   { dynamicDecorationInitialEncodedField :: d
   , metadata                             :: DiscreteCharacterMetadataDec (Element d)
   }


-- | (✔)
instance ( EncodableStreamElement (Element d)
         , MonoFoldable d
         , PossiblyMissingCharacter d
         ) => Show (DynamicDecorationInitial d) where

    show dec
      | isMissing character = "<Missing>"
      | otherwise           = ofoldMap (showStreamElement alphabet) character
      where
        character = dec ^. encoded
        alphabet  = dec ^. characterAlphabet


-- | (✔)
instance PossiblyMissingCharacter d => PossiblyMissingCharacter (DynamicDecorationInitial d) where

    isMissing = isMissing . (^. encoded)

    toMissing x = x & encoded %~ toMissing


-- | (✔)
instance HasEncoded (DynamicDecorationInitial d) d where

    encoded = lens dynamicDecorationInitialEncodedField (\e x -> e { dynamicDecorationInitialEncodedField = x })


-- | (✔)
instance GeneralCharacterMetadata (DynamicDecorationInitial d) where


-- | (✔)
instance HasCharacterAlphabet (DynamicDecorationInitial d) (Alphabet String) where

    characterAlphabet = lens getter setter
      where
         getter e   = metadata e ^. characterAlphabet
         setter e x = e { metadata = metadata e &  characterAlphabet .~ x }


-- | (✔)
instance HasCharacterName (DynamicDecorationInitial d) CharacterName where

    characterName = lens getter setter
      where
         getter e   = metadata e ^. characterName
         setter e x = e { metadata = metadata e &  characterName .~ x }


-- |
-- A 'Lens' for the 'symbolicTCMGenerator' field
instance HasCharacterSymbolTransitionCostMatrixGenerator (DynamicDecorationInitial d) (Int -> Int -> Int) where

    characterSymbolTransitionCostMatrixGenerator = lens getter setter
      where
         getter e   = metadata e ^. characterSymbolTransitionCostMatrixGenerator
         setter e f = e { metadata = metadata e &  characterSymbolTransitionCostMatrixGenerator .~ f }


-- |
-- A 'Lens' for the 'transitionCostMatrix' field
instance (Element d ~ c) => HasCharacterTransitionCostMatrix (DynamicDecorationInitial d) (c -> c -> (c, Int)) where

    characterTCM = lens getter setter
      where
         getter e   = metadata e ^. characterTCM
         setter e f = e { metadata = metadata e &  characterTCM .~ f }


-- | (✔)
instance HasCharacterWeight (DynamicDecorationInitial d) Double where

    characterWeight = lens getter setter
      where
         getter e   = metadata e ^. characterWeight
         setter e x = e { metadata = metadata e &  characterWeight .~ x }


-- | (✔)
instance (EncodableStream d, Element d ~ c) => DiscreteCharacterMetadata (DynamicDecorationInitial d) c where


-- | (✔)
instance (EncodableDynamicCharacter d) => SimpleDynamicDecoration (DynamicDecorationInitial d) d where


-- | (✔)
instance (EncodableDynamicCharacter d) => DynamicCharacterDecoration (DynamicDecorationInitial d) d where

--    toDynamicCharacterDecoration :: CharacterName -> Double -> Alphabet String -> TCM -> (x -> a) -> x -> s
    toDynamicCharacterDecoration name weight alphabet tcm g symbolSet =
        DynamicDecorationInitial
        { dynamicDecorationInitialEncodedField = g symbolSet
        , metadata                             = discreteMetadata name weight alphabet tcm
        }



data DynamicDecorationDirectOptimizationPostOrderResult d
   = DynamicDecorationDirectOptimizationPostOrderResult
   { dynamicDecorationDirectOptimizationPostOrderEncodedField             :: d
   , dynamicDecorationDirectOptimizationPostOrderPreliminaryGappedField   :: d
   , dynamicDecorationDirectOptimizationPostOrderPreliminaryUngappedField :: d
   , dynamicDecorationDirectOptimizationPostOrderMetadata                 :: DiscreteCharacterMetadataDec (Element d)
   }                               


-- | (✔)
instance HasEncoded (DynamicDecorationDirectOptimizationPostOrderResult d) d where

    encoded = lens dynamicDecorationDirectOptimizationPostOrderEncodedField (\e x -> e { dynamicDecorationDirectOptimizationPostOrderEncodedField = x })
      

-- | (✔)
instance HasPreliminaryGapped (DynamicDecorationDirectOptimizationPostOrderResult d) d where

    preliminaryGapped = lens dynamicDecorationDirectOptimizationPostOrderPreliminaryGappedField (\e x -> e { dynamicDecorationDirectOptimizationPostOrderPreliminaryGappedField = x })


-- | (✔)
instance HasPreliminaryUngapped (DynamicDecorationDirectOptimizationPostOrderResult d) d where

    preliminaryUngapped = lens dynamicDecorationDirectOptimizationPostOrderPreliminaryUngappedField (\e x -> e { dynamicDecorationDirectOptimizationPostOrderPreliminaryUngappedField = x })


-- | (✔)
instance HasCharacterAlphabet (DynamicDecorationDirectOptimizationPostOrderResult d) (Alphabet String) where

    characterAlphabet = lens getter setter
      where
         getter e   = dynamicDecorationDirectOptimizationPostOrderMetadata e ^. characterAlphabet
         setter e x = e { dynamicDecorationDirectOptimizationPostOrderMetadata = dynamicDecorationDirectOptimizationPostOrderMetadata e &  characterAlphabet .~ x }


-- | (✔)
instance HasCharacterName (DynamicDecorationDirectOptimizationPostOrderResult d) CharacterName where

    characterName = lens getter setter
      where
         getter e   = dynamicDecorationDirectOptimizationPostOrderMetadata e ^. characterName
         setter e x = e { dynamicDecorationDirectOptimizationPostOrderMetadata = dynamicDecorationDirectOptimizationPostOrderMetadata e &  characterName .~ x }


-- |
-- A 'Lens' for the 'symbolicTCMGenerator' field
instance HasCharacterSymbolTransitionCostMatrixGenerator (DynamicDecorationDirectOptimizationPostOrderResult d) (Int -> Int -> Int) where

    characterSymbolTransitionCostMatrixGenerator = lens getter setter
      where
         getter e   = dynamicDecorationDirectOptimizationPostOrderMetadata e ^. characterSymbolTransitionCostMatrixGenerator
         setter e f = e { dynamicDecorationDirectOptimizationPostOrderMetadata = dynamicDecorationDirectOptimizationPostOrderMetadata e &  characterSymbolTransitionCostMatrixGenerator .~ f }


-- |
-- A 'Lens' for the 'transitionCostMatrix' field
instance (Element d ~ c) => HasCharacterTransitionCostMatrix (DynamicDecorationDirectOptimizationPostOrderResult d) (c -> c -> (c, Int)) where

    characterTCM = lens getter setter
      where
         getter e   = dynamicDecorationDirectOptimizationPostOrderMetadata e ^. characterTCM
         setter e f = e { dynamicDecorationDirectOptimizationPostOrderMetadata = dynamicDecorationDirectOptimizationPostOrderMetadata e &  characterTCM .~ f }


-- | (✔)
instance HasCharacterWeight (DynamicDecorationDirectOptimizationPostOrderResult d) Double where

    characterWeight = lens getter setter
      where
         getter e   = dynamicDecorationDirectOptimizationPostOrderMetadata e ^. characterWeight
         setter e x = e { dynamicDecorationDirectOptimizationPostOrderMetadata = dynamicDecorationDirectOptimizationPostOrderMetadata e &  characterWeight .~ x }


-- | (✔)
instance EncodableDynamicCharacter d => SimpleDynamicDecoration (DynamicDecorationDirectOptimizationPostOrderResult d) d where

  
-- | (✔)
instance GeneralCharacterMetadata (DynamicDecorationDirectOptimizationPostOrderResult d) where


-- | (✔)
instance (EncodableStream d, Element d ~ c) => DiscreteCharacterMetadata (DynamicDecorationDirectOptimizationPostOrderResult d) c where

  



-- |
-- An abstract direct optimization dynamic character decoration with a
-- polymorphic character type.
data DynamicDecorationDirectOptimization d
   = DynamicDecorationDirectOptimization
   { dynamicDecorationDirectOptimizationEncodedField             :: d
   , dynamicDecorationDirectOptimizationFinalGappedField         :: d
   , dynamicDecorationDirectOptimizationFinalUngappedField       :: d
   , dynamicDecorationDirectOptimizationPreliminaryGappedField   :: d
   , dynamicDecorationDirectOptimizationPreliminaryUngappedField :: d
   , dynamicDecorationDirectOptimizationMetadata                 :: DiscreteCharacterMetadataDec (Element d)
   }


-- | (✔)
instance HasEncoded (DynamicDecorationDirectOptimization d) d where

    encoded = lens dynamicDecorationDirectOptimizationEncodedField (\e x -> e { dynamicDecorationDirectOptimizationEncodedField = x })


-- | (✔)
instance HasFinalGapped (DynamicDecorationDirectOptimization d) d where

    finalGapped = lens dynamicDecorationDirectOptimizationFinalGappedField (\e x -> e { dynamicDecorationDirectOptimizationFinalGappedField = x })


-- | (✔)
instance HasFinalUngapped (DynamicDecorationDirectOptimization d) d where

    finalUngapped = lens dynamicDecorationDirectOptimizationFinalUngappedField (\e x -> e { dynamicDecorationDirectOptimizationFinalUngappedField = x })


-- | (✔)
instance HasPreliminaryGapped (DynamicDecorationDirectOptimization d) d where

    preliminaryGapped = lens dynamicDecorationDirectOptimizationPreliminaryGappedField (\e x -> e { dynamicDecorationDirectOptimizationPreliminaryGappedField = x })


-- | (✔)
instance HasPreliminaryUngapped (DynamicDecorationDirectOptimization d) d where

    preliminaryUngapped = lens dynamicDecorationDirectOptimizationPreliminaryUngappedField (\e x -> e { dynamicDecorationDirectOptimizationPreliminaryUngappedField = x })


-- | (✔)
instance HasCharacterAlphabet (DynamicDecorationDirectOptimization d) (Alphabet String) where

    characterAlphabet = lens getter setter
      where
         getter e   = dynamicDecorationDirectOptimizationMetadata e ^. characterAlphabet
         setter e x = e { dynamicDecorationDirectOptimizationMetadata = dynamicDecorationDirectOptimizationMetadata e &  characterAlphabet .~ x }


-- | (✔)
instance HasCharacterName (DynamicDecorationDirectOptimization d) CharacterName where

    characterName = lens getter setter
      where
         getter e   = dynamicDecorationDirectOptimizationMetadata e ^. characterName
         setter e x = e { dynamicDecorationDirectOptimizationMetadata = dynamicDecorationDirectOptimizationMetadata e &  characterName .~ x }


-- |
-- A 'Lens' for the 'symbolicTCMGenerator' field
instance HasCharacterSymbolTransitionCostMatrixGenerator (DynamicDecorationDirectOptimization d) (Int -> Int -> Int) where

    characterSymbolTransitionCostMatrixGenerator = lens getter setter
      where
         getter e   = dynamicDecorationDirectOptimizationMetadata e ^. characterSymbolTransitionCostMatrixGenerator
         setter e f = e { dynamicDecorationDirectOptimizationMetadata = dynamicDecorationDirectOptimizationMetadata e &  characterSymbolTransitionCostMatrixGenerator .~ f }


-- |
-- A 'Lens' for the 'transitionCostMatrix' field
instance (Element d ~ c) => HasCharacterTransitionCostMatrix (DynamicDecorationDirectOptimization d) (c -> c -> (c, Int)) where

    characterTCM = lens getter setter
      where
         getter e   = dynamicDecorationDirectOptimizationMetadata e ^. characterTCM
         setter e f = e { dynamicDecorationDirectOptimizationMetadata = dynamicDecorationDirectOptimizationMetadata e &  characterTCM .~ f }


-- | (✔)
instance HasCharacterWeight (DynamicDecorationDirectOptimization d) Double where

    characterWeight = lens getter setter
      where
         getter e   = dynamicDecorationDirectOptimizationMetadata e ^. characterWeight
         setter e x = e { dynamicDecorationDirectOptimizationMetadata = dynamicDecorationDirectOptimizationMetadata e &  characterWeight .~ x }


-- | (✔)
instance EncodableDynamicCharacter d => SimpleDynamicDecoration (DynamicDecorationDirectOptimization d) d where


-- | (✔)
instance EncodableDynamicCharacter d => DirectOptimizationPostOrderDecoration (DynamicDecorationDirectOptimization d) d where


-- | (✔)
instance EncodableDynamicCharacter d => DirectOptimizationDecoration (DynamicDecorationDirectOptimization d) d where


-- | (✔)
instance GeneralCharacterMetadata (DynamicDecorationDirectOptimization d) where


-- | (✔)
instance (EncodableStream d, Element d ~ c) => DiscreteCharacterMetadata (DynamicDecorationDirectOptimization d) c where







-- |
-- An abstract implied alignment dynamic character decoration with a polymorphic
-- character type.
data DynamicDecorationImpliedAlignment d
   = DynamicDecorationImpliedAlignment
   { dynamicDecorationImpliedAlignmentEncodedField             :: d
   , dynamicDecorationImpliedAlignmentFinalGappedField         :: d
   , dynamicDecorationImpliedAlignmentFinalUngappedField       :: d
   , dynamicDecorationImpliedAlignmentPreliminaryGappedField   :: d
   , dynamicDecorationImpliedAlignmentPreliminaryUngappedField :: d
   , dynamicDecorationImpliedAlignmentImpliedAlignmentField    :: d
   , dynamicDecorationImpliedAlignmentMetadata                 :: DiscreteCharacterMetadataDec (Element d)
   }


-- | (✔)
instance HasEncoded (DynamicDecorationImpliedAlignment d) d where

    encoded = lens dynamicDecorationImpliedAlignmentEncodedField (\e x -> e { dynamicDecorationImpliedAlignmentEncodedField = x })


-- | (✔)
instance HasFinalGapped (DynamicDecorationImpliedAlignment d) d where

    finalGapped = lens dynamicDecorationImpliedAlignmentFinalGappedField (\e x -> e { dynamicDecorationImpliedAlignmentFinalGappedField = x })


-- | (✔)
instance HasFinalUngapped (DynamicDecorationImpliedAlignment d) d where

    finalUngapped = lens dynamicDecorationImpliedAlignmentFinalUngappedField (\e x -> e { dynamicDecorationImpliedAlignmentFinalUngappedField = x })


-- | (✔)
instance HasPreliminaryGapped (DynamicDecorationImpliedAlignment d) d where

    preliminaryGapped = lens dynamicDecorationImpliedAlignmentPreliminaryGappedField (\e x -> e { dynamicDecorationImpliedAlignmentPreliminaryGappedField = x })


-- | (✔)
instance HasPreliminaryUngapped (DynamicDecorationImpliedAlignment d) d where

    preliminaryUngapped = lens dynamicDecorationImpliedAlignmentPreliminaryUngappedField (\e x -> e { dynamicDecorationImpliedAlignmentPreliminaryUngappedField = x })


-- | (✔)
instance HasImpliedAlignment (DynamicDecorationImpliedAlignment d) d where

    impliedAlignment = lens dynamicDecorationImpliedAlignmentImpliedAlignmentField (\e x -> e { dynamicDecorationImpliedAlignmentImpliedAlignmentField = x })


-- | (✔)
instance HasCharacterAlphabet (DynamicDecorationImpliedAlignment d) (Alphabet String) where

    characterAlphabet = lens getter setter
      where
         getter e   = dynamicDecorationImpliedAlignmentMetadata e ^. characterAlphabet
         setter e x = e { dynamicDecorationImpliedAlignmentMetadata = dynamicDecorationImpliedAlignmentMetadata e &  characterAlphabet .~ x }


-- | (✔)
instance HasCharacterName (DynamicDecorationImpliedAlignment d) CharacterName where

    characterName = lens getter setter
      where
         getter e   = dynamicDecorationImpliedAlignmentMetadata e ^. characterName
         setter e x = e { dynamicDecorationImpliedAlignmentMetadata = dynamicDecorationImpliedAlignmentMetadata e &  characterName .~ x }


-- |
-- A 'Lens' for the 'symbolicTCMGenerator' field
instance HasCharacterSymbolTransitionCostMatrixGenerator (DynamicDecorationImpliedAlignment d) (Int -> Int -> Int) where

    characterSymbolTransitionCostMatrixGenerator = lens getter setter
      where
         getter e   = dynamicDecorationImpliedAlignmentMetadata e ^. characterSymbolTransitionCostMatrixGenerator
         setter e f = e { dynamicDecorationImpliedAlignmentMetadata = dynamicDecorationImpliedAlignmentMetadata e &  characterSymbolTransitionCostMatrixGenerator .~ f }


-- |
-- A 'Lens' for the 'transitionCostMatrix' field
instance (Element d ~ c) => HasCharacterTransitionCostMatrix (DynamicDecorationImpliedAlignment d) (c -> c -> (c, Int)) where

    characterTCM = lens getter setter
      where
         getter e   = dynamicDecorationImpliedAlignmentMetadata e ^. characterTCM
         setter e f = e { dynamicDecorationImpliedAlignmentMetadata = dynamicDecorationImpliedAlignmentMetadata e &  characterTCM .~ f }


-- | (✔)
instance HasCharacterWeight (DynamicDecorationImpliedAlignment d) Double where

    characterWeight = lens getter setter
      where
         getter e   = dynamicDecorationImpliedAlignmentMetadata e ^. characterWeight
         setter e x = e { dynamicDecorationImpliedAlignmentMetadata = dynamicDecorationImpliedAlignmentMetadata e &  characterWeight .~ x }


-- | (✔)
instance GeneralCharacterMetadata (DynamicDecorationImpliedAlignment d) where


-- | (✔)
instance (EncodableStream d, Element d ~ c) => DiscreteCharacterMetadata (DynamicDecorationImpliedAlignment d) c where

-- | (✔)
instance EncodableDynamicCharacter d => SimpleDynamicDecoration (DynamicDecorationImpliedAlignment d) d where


-- | (✔)
instance EncodableDynamicCharacter d => DirectOptimizationPostOrderDecoration (DynamicDecorationImpliedAlignment d) d where


-- | (✔)
instance EncodableDynamicCharacter d => DirectOptimizationDecoration (DynamicDecorationImpliedAlignment d) d where


-- | (✔)
instance EncodableDynamicCharacter d => ImpliedAlignmentDecoration   (DynamicDecorationImpliedAlignment d) d where
