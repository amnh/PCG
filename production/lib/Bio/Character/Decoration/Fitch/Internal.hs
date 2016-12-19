-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Character.Decoration.Fitch.Internal
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, TypeFamilies #-}

module Bio.Character.Decoration.Fitch.Internal where


import Bio.Character.Decoration.Discrete
import Bio.Character.Decoration.Fitch.Class
import Bio.Character.Encodable
import Bio.Metadata.CharacterName
import Bio.Metadata.Discrete
import Control.Lens
import Data.Alphabet
import Data.TCM
--import Data.Word


-- |
-- An abstract initial dynamic character decoration with a polymorphic character
-- type.
data FitchOptimizationDecoration f
   = FitchOptimizationDecoration
   { fitchMinCost           :: Word                                        -- Cost of the subtree
   , fitchPreliminaryMedian :: StaticCharacter                             -- Held here until final state is
                                                                           --     determined and we can assign that
                                                                           --     into discreteCharacter
   , fitchFinalMedian       :: StaticCharacter                             -- Eventually gets assigned to discreteCharacter
   , fitchChildMedians      :: (StaticCharacter, StaticCharacter)          -- (left, right) so that we can do post order
                                                                           --     pass with all of Fitch's rules
   , fitchIsLeaf            :: Bool                                        -- need this in preorder
   , fitchCharacterField    :: f
   , fitchMetadataField     :: DiscreteCharacterMetadataDec f
   }


-- | (✔)
instance HasDiscreteCharacter (FitchOptimizationDecoration f) f where

    discreteCharacter = lens fitchCharacterField (\e x -> e { fitchCharacterField = x })


-- | (✔)
instance HasCharacterAlphabet (FitchOptimizationDecoration f) (Alphabet String) where

    characterAlphabet = lens getter setter
      where
         getter e   = fitchMetadataField e ^. characterAlphabet
         setter e x = e { fitchMetadataField = fitchMetadataField e &  characterAlphabet .~ x }


-- | (✔)
instance HasCharacterName (FitchOptimizationDecoration f) CharacterName where

    characterName = lens getter setter
      where
         getter e   = fitchMetadataField e ^. characterName
         setter e x = e { fitchMetadataField = fitchMetadataField e &  characterName .~ x }


-- |
-- A 'Lens' for the 'symbolicTCMGenerator' field
instance HasCharacterSymbolTransitionCostMatrixGenerator (FitchOptimizationDecoration f) (Int -> Int -> Int) where

    characterSymbolTransitionCostMatrixGenerator = lens getter setter
      where
         getter e   = fitchMetadataField e ^. characterSymbolTransitionCostMatrixGenerator
         setter e f = e { fitchMetadataField = fitchMetadataField e &  characterSymbolTransitionCostMatrixGenerator .~ f }


-- |
-- A 'Lens' for the 'transitionCostMatrix' field
instance HasCharacterTransitionCostMatrix (FitchOptimizationDecoration f) (f -> f -> (f, Int)) where

    characterTCM = lens getter setter
      where
         getter e   = fitchMetadataField e ^. characterTCM
         setter e f = e { fitchMetadataField = fitchMetadataField e &  characterTCM .~ f }


-- | (✔)
instance HasCharacterWeight (FitchOptimizationDecoration f) Double where

    characterWeight = lens getter setter
      where
         getter e   = fitchMetadataField e ^. characterWeight
         setter e x = e { fitchMetadataField = fitchMetadataField e &  characterWeight .~ x }

-- | (✔)
instance HasIsLeaf (FitchOptimizationDecoration f) Bool where

    isLeaf = lens fitchIsLeaf (\e x -> e { fitchIsLeaf = x })

-- | (✔)
instance HasMinCost (FitchOptimizationDecoration f) Word where

    minCost = lens fitchMinCost (\e x -> e { fitchMinCost = x })

-- | (✔)
instance HasPreliminaryMedian (FitchOptimizationDecoration f) StaticCharacter where
    preliminaryMedian = lens fitchPreliminaryMedian (\e x -> e { fitchPreliminaryMedian = x })

-- | (✔)
instance HasChildMedians (FitchOptimizationDecoration f) ( StaticCharacter, StaticCharacter ) where
    childMedians = lens fitchChildMedians (\e x -> e { fitchChildMedians = x })

-- | (✔)
instance HasFinalMedian (FitchOptimizationDecoration f) StaticCharacter where
    finalMedian = lens fitchFinalMedian (\e x -> e { fitchFinalMedian = x })


-- | (✔)
instance GeneralCharacterMetadata (FitchOptimizationDecoration f) where


-- | (✔)
instance EncodableStreamElement f => DiscreteCharacterMetadata (FitchOptimizationDecoration f) f where


-- | (✔)
instance EncodableStaticCharacter f => DiscreteCharacterDecoration (FitchOptimizationDecoration f) f where


-- | (✔)
instance EncodableStaticCharacter f => FitchCharacterDecoration (FitchOptimizationDecoration f) f where


-- | (✔)
instance EncodableStaticCharacter f => FitchDecoration (FitchOptimizationDecoration f) f where


-- | (✔)
instance EncodableStaticCharacter f => DiscreteExtensionFitchDecoration (FitchOptimizationDecoration f) f where

    extendDiscreteToFitch subDecoration cost prelimMedian finMedian childMedianTup isLeafVal =

        FitchOptimizationDecoration
        { fitchChildMedians      = childMedianTup
        , fitchIsLeaf            = isLeafVal
        , fitchMinCost           = cost
        , fitchMetadataField     = metadataValue
        , fitchPreliminaryMedian = prelimMedian
        , fitchFinalMedian       = finMedian
        , fitchCharacterField    = subDecoration ^. discreteCharacter
        }
      where
        alphabetValue = subDecoration ^. characterAlphabet
        tcmValue      = generate (length alphabetValue) (uncurry $ subDecoration ^. characterSymbolTransitionCostMatrixGenerator)
        metadataValue =
          discreteMetadata
            <$> (^. characterName)
            <*> (^. characterWeight)
            <*> const alphabetValue
            <*> const tcmValue
            $ subDecoration


