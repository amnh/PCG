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
import Bio.Character.Decoration.Shared
import Bio.Character.Encodable
import Bio.Metadata.CharacterName
import Bio.Metadata.Discrete
import Bio.Metadata.DiscreteWithTCM
import Control.Lens
import Data.Alphabet
import Data.Semigroup


-- |
-- An abstract initial dynamic character decoration with a polymorphic character
-- type.
data FitchOptimizationDecoration f
   = FitchOptimizationDecoration
   { fitchMinCost           :: Word                          -- Cost of the subtree
   , fitchPreliminaryMedian :: f                             -- Held here until final state is
                                                             --     determined and we can assign that
                                                             --     into discreteCharacter
   , fitchFinalMedian       :: f                             -- Eventually gets assigned to discreteCharacter
   , fitchChildMedians      :: (f, f)                        -- (left, right) so that we can do post order
                                                             --     pass with all of Fitch's rules
   , fitchIsLeaf            :: Bool                          -- need this in preorder
   , fitchCharacterField    :: f
   , fitchMetadataField     :: DiscreteCharacterMetadataDec
   }


-- | (✔)
instance EncodableStreamElement c => Show (FitchOptimizationDecoration c) where

    show c = unlines
        [ {- "Cost = " <> show (fitchMinCost c)
        , "Is Leaf Node?      : " <> show (fitchIsLeaf c)
        , -} "Discrete Character : " <> showDiscreteCharacterElement c
        , "Preliminary Median : " <> showStatic (fitchPreliminaryMedian  c)
        , "Final       Median : " <> showStatic (fitchFinalMedian        c)
        -- , mconcat ["Child       Medians: ( ", (showStatic . fst . fitchChildMedians) c, " , ", (showStatic . snd . fitchChildMedians) c, " )"]
        ]
      where
        alphabet = c ^. characterAlphabet
        showStatic x
          | x == 0    = "<Empty Character>"
          | otherwise = showStreamElement alphabet x


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
instance HasSymbolChangeMatrix (FitchOptimizationDecoration f) (Word -> Word -> Word) where

    symbolChangeMatrix = lens getter setter
      where
         getter = const $ \i j -> if i == j then 0 else 1
         setter = const


-- |
-- A 'Lens' for the 'transitionCostMatrix' field
instance HasTransitionCostMatrix (FitchOptimizationDecoration f) (f -> f -> (f, Word)) where

    -- NOTE: This probably isn't sound
    transitionCostMatrix = lens getter setter
      where
        getter = error "Please don't use lens accessor operations over 'transitionCostMatrix' on a FitchOptimizationDecoration."
        setter = const


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
instance HasCharacterCost (FitchOptimizationDecoration f) Word where

    characterCost = lens fitchMinCost (\e x -> e { fitchMinCost = x })


-- | (✔)
instance HasPreliminaryMedian (FitchOptimizationDecoration f) f where

    preliminaryMedian = lens fitchPreliminaryMedian (\e x -> e { fitchPreliminaryMedian = x })


-- | (✔)
instance HasChildMedians (FitchOptimizationDecoration f) ( f, f ) where

    childMedians = lens fitchChildMedians (\e x -> e { fitchChildMedians = x })


-- | (✔)
instance HasFinalMedian (FitchOptimizationDecoration f) f where

    finalMedian = lens fitchFinalMedian (\e x -> e { fitchFinalMedian = x })


-- | (✔)
instance GeneralCharacterMetadata (FitchOptimizationDecoration f) where

    extractGeneralCharacterMetadata = extractGeneralCharacterMetadata . fitchMetadataField


-- | (✔)
instance DiscreteCharacterMetadata (FitchOptimizationDecoration f) where

    extractDiscreteCharacterMetadata = fitchMetadataField
  

-- | (✔)
instance EncodableStaticCharacter f => DiscreteWithTcmCharacterMetadata (FitchOptimizationDecoration f) f


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
--        tcmValue      = generate (length alphabetValue) (uncurry $ subDecoration ^. characterSymbolTransitionCostMatrixGenerator)
        metadataValue =
          discreteMetadata
            <$> (^. characterName)
            <*> (^. characterWeight)
            <*> const alphabetValue
--            <*> const tcmValue
            $ subDecoration


