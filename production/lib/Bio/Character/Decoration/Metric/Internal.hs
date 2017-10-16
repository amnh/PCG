-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Character.Decoration.Metric.Internal
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, TypeFamilies #-}

module Bio.Character.Decoration.Metric.Internal where


import Bio.Character.Decoration.Metric.Class
import Bio.Character.Decoration.Discrete
import Bio.Character.Decoration.Shared
import Bio.Character.Encodable
import Bio.Metadata.CharacterName
--import Bio.Metadata.Discrete
import Bio.Metadata.DiscreteWithTCM
import Control.Lens
import Data.Alphabet
import Data.TCM
import Numeric.Extended.Natural
import Text.XML


-- |
-- An abstract initial dynamic character decoration with a polymorphic character
-- type.
data MetricDecorationInitial c
   = MetricDecorationInitial
   { metricDecorationInitialCharacter :: c
   , metadata                         :: DiscreteWithTCMCharacterMetadataDec c
   }

-- |
-- A concrete type representing the results of performing Sankoff's algorithm.
data SankoffOptimizationDecoration c
   = SankoffOptimizationDecoration
   { sankoffMinStateTuple  :: ([StateContributionList], [StateContributionList]) -- tuple of (a,a) where a is a per-parent-state list of
                                                                                 -- lists of child states that contributed to the minimum
                                                                                 -- cost of that state
   , sankoffMinCostVector         :: [ExtendedNatural]                           -- minimum total cost per state (left + right)
   , sankoffMinCost               :: Word                                        -- overall minimum cost for all states
   , sankoffPreliminaryExtraCosts :: [ExtendedNatural]                           -- list of preliminary per-character-state extra costs
                                                                                 -- for the node; these store only the costs for assigning
                                                                                 -- this state to THIS node, rather than the accumulated
                                                                                 -- extra costs down through the tree when this assignment
                                                                                 -- is chosen
   , sankoffFinalExtraCosts       :: [ExtendedNatural]                           -- list of final extra costs for the node, so the sum of
                                                                                 -- ALL of the extra costs, on the whole tree for this a
                                                                                 -- assignment
   , sankoffBeta                  :: [ExtendedNatural]                           -- this is Goloboff's beta, where
                                                                                 -- beta_(s,n) = min[t_(s,x) + prelimExtraCost_(x,n)]
                                                                                 -- where t_(s,x) is the transition cost from state s to x
   , sankoffMetadataField         :: DiscreteWithTCMCharacterMetadataDec c
   , sankoffCharacterField        :: c                                           -- Bit Vector version of median character
   , sankoffIsLeaf                :: Bool
   }


-- | A list of states on the child that contribute to the lowest score on each state in the parent
-- Used to simplify? SankoffOptimizationDecoration
type StateContributionList = [Word]



-- | (✔)
instance EncodableStaticCharacter c => DiscreteCharacterDecoration (MetricDecorationInitial c) c where


-- | (✔)
instance EncodableStaticCharacter c => DiscreteCharacterDecoration (SankoffOptimizationDecoration c) c where


-- | (✔)
instance DiscreteCharacterMetadata (MetricDecorationInitial c) where

    extractDiscreteCharacterMetadata = extractDiscreteCharacterMetadata . metadata


-- | (✔)
instance DiscreteCharacterMetadata (SankoffOptimizationDecoration c) where

    extractDiscreteCharacterMetadata = extractDiscreteCharacterMetadata . sankoffMetadataField


-- | (✔)
instance EncodableStaticCharacter c => DiscreteExtensionSankoffDecoration (SankoffOptimizationDecoration c) c where

--    extendDiscreteToSankoff :: DiscreteCharacterDecoration x c => x -> [Word] -> ([Word], [Word]) -> Word -> s
    extendDiscreteToSankoff subDecoration costVector prelimExtras finalExtras inputBeta childMinStates cost newMedian leaf =

        SankoffOptimizationDecoration
        { sankoffMinStateTuple         = childMinStates
        , sankoffMinCostVector         = costVector
        , sankoffPreliminaryExtraCosts = prelimExtras
        , sankoffFinalExtraCosts       = finalExtras
        , sankoffBeta                  = inputBeta
        , sankoffMetadataField         = metadataValue
        , sankoffMinCost               = cost
        , sankoffCharacterField        = newMedian
        , sankoffIsLeaf                = leaf
        }
      where
        alphabetValue   = subDecoration ^. characterAlphabet
        tcmValue        = generate (length alphabetValue) generator
        generator (i,j) = (subDecoration ^. symbolChangeMatrix) (toEnum i) (toEnum j)
        metadataValue   =
            discreteMetadataFromTCM
                <$> (^. characterName)
                <*> (^. characterWeight)
                <*> const alphabetValue
                <*> const tcmValue
                 $  subDecoration


-- | (✔)
instance EncodableStreamElement c => DiscreteWithTcmCharacterMetadata (MetricDecorationInitial c) c where


-- | (✔)
instance EncodableStreamElement c => DiscreteWithTcmCharacterMetadata (SankoffOptimizationDecoration c) c where


-- | (✔)
instance GeneralCharacterMetadata (MetricDecorationInitial c) where

    extractGeneralCharacterMetadata = extractGeneralCharacterMetadata . metadata


-- | (✔)
instance GeneralCharacterMetadata (SankoffOptimizationDecoration c) where

    extractGeneralCharacterMetadata = extractGeneralCharacterMetadata . sankoffMetadataField


-- | (✔)
instance HasBeta (SankoffOptimizationDecoration c) [ExtendedNatural] where

    beta = lens sankoffBeta (\e x -> e { sankoffBeta = x })


-- | (✔)
instance HasCharacterAlphabet (MetricDecorationInitial c) (Alphabet String) where

    characterAlphabet = lens getter setter
      where
         getter e   = metadata e ^. characterAlphabet
         setter e x = e { metadata = metadata e &  characterAlphabet .~ x }


-- | (✔)
instance HasCharacterAlphabet (SankoffOptimizationDecoration c) (Alphabet String) where

    characterAlphabet = lens getter setter
      where
         getter e   = sankoffMetadataField e ^. characterAlphabet
         setter e x = e { sankoffMetadataField = sankoffMetadataField e &  characterAlphabet .~ x }


-- | (✔)
instance HasCharacterCost (SankoffOptimizationDecoration c) Word where

    characterCost = lens sankoffMinCost (\e x -> e { sankoffMinCost = x })


-- | (✔)
instance HasCharacterCostVector (SankoffOptimizationDecoration c) [ExtendedNatural] where

    characterCostVector = lens sankoffMinCostVector (\e x -> e { sankoffMinCostVector = x })


-- | (✔)
instance HasCharacterName (MetricDecorationInitial c) CharacterName where

    characterName = lens getter setter
      where
         getter e   = metadata e ^. characterName
         setter e x = e { metadata = metadata e &  characterName .~ x }


-- | (✔)
instance HasCharacterName (SankoffOptimizationDecoration c) CharacterName where

    characterName = lens getter setter
      where
         getter e   = sankoffMetadataField e ^. characterName
         setter e x = e { sankoffMetadataField = sankoffMetadataField e &  characterName .~ x }


-- | (✔)
instance HasCharacterWeight (MetricDecorationInitial c) Double where

    characterWeight = lens getter setter
      where
         getter e   = metadata e ^. characterWeight
         setter e x = e { metadata = metadata e &  characterWeight .~ x }


-- | (✔)
instance HasCharacterWeight (SankoffOptimizationDecoration c) Double where

    characterWeight = lens getter setter
      where
         getter e   = sankoffMetadataField e ^. characterWeight
         setter e x = e { sankoffMetadataField = sankoffMetadataField e &  characterWeight .~ x }


-- | (✔)
instance HasDiscreteCharacter (MetricDecorationInitial c) c where

    discreteCharacter = lens metricDecorationInitialCharacter (\e x -> e { metricDecorationInitialCharacter = x })


-- | (✔)
instance HasDiscreteCharacter (SankoffOptimizationDecoration c) c where

    discreteCharacter = lens sankoffCharacterField (\e x -> e { sankoffCharacterField = x })


-- | (✔)
instance HasFinalExtraCost (SankoffOptimizationDecoration c) [ExtendedNatural] where

    finalExtraCost = lens sankoffFinalExtraCosts (\e x -> e { sankoffFinalExtraCosts = x })


-- | (✔)
instance HasIsLeaf (SankoffOptimizationDecoration c) Bool where

    isLeaf = lens sankoffIsLeaf (\e x -> e { sankoffIsLeaf = x })


-- | (✔)
instance HasPreliminaryExtraCost (SankoffOptimizationDecoration c) [ExtendedNatural] where

    preliminaryExtraCost = lens sankoffPreliminaryExtraCosts (\e x -> e { sankoffPreliminaryExtraCosts = x })


-- | (✔)
instance HasStateMinTuple (SankoffOptimizationDecoration c) ([StateContributionList], [StateContributionList]) where

    minStateTuple = lens sankoffMinStateTuple (\e x -> e { sankoffMinStateTuple = x })


-- | (✔)
instance HasSymbolChangeMatrix (MetricDecorationInitial c) (Word -> Word -> Word) where

    symbolChangeMatrix = lens getter setter
      where
         getter e   = metadata e ^. symbolChangeMatrix
         setter e f = e { metadata = metadata e & symbolChangeMatrix .~ f }


-- | (✔)
instance HasSymbolChangeMatrix (SankoffOptimizationDecoration c) (Word -> Word -> Word) where

    symbolChangeMatrix = lens getter setter
      where
         getter e   = sankoffMetadataField e ^. symbolChangeMatrix
         setter e f = e { sankoffMetadataField = sankoffMetadataField e & symbolChangeMatrix .~ f }


-- | (✔)
instance HasTransitionCostMatrix (MetricDecorationInitial c) (c -> c -> (c, Word)) where

    transitionCostMatrix = lens getter setter
      where
         getter e   = metadata e ^. transitionCostMatrix
         setter e f = e { metadata = metadata e &  transitionCostMatrix .~ f }


-- | (✔)
instance HasTransitionCostMatrix (SankoffOptimizationDecoration c) (c -> c -> (c, Word)) where

    transitionCostMatrix = lens getter setter
      where
         getter e   = sankoffMetadataField e ^. transitionCostMatrix
         setter e f = e { sankoffMetadataField = sankoffMetadataField e & transitionCostMatrix .~ f }


-- | (✔)
instance EncodableStaticCharacter c => MetricCharacterDecoration (MetricDecorationInitial c) c where


-- | (✔)
instance EncodableStaticCharacter c => MetricCharacterDecoration (SankoffOptimizationDecoration c) c where


-- | (✔)
instance EncodableStaticCharacter c => SankoffDecoration (SankoffOptimizationDecoration c) c where


-- | (✔)
instance EncodableStreamElement c => Show (SankoffOptimizationDecoration c) where

    show = showDiscreteCharacterElement


-- | (✔)
instance EncodableStaticCharacter c => SimpleDiscreteCharacterDecoration (MetricDecorationInitial c) c where

    toDiscreteCharacterDecoration name weight alphabet scm g symbolSet =
        MetricDecorationInitial
        { metricDecorationInitialCharacter = g symbolSet
        , metadata                         = discreteMetadataWithTCM name weight alphabet scm
        }


-- |
instance ToXML (SankoffOptimizationDecoration c) where

    toXML metricDecoration = xmlElement "Sankoff_optimization_decoration" attributes contents
        where
            attributes = []
            contents   = [ Left ("Min_cost_vector", show $ metricDecoration ^. characterCost      )
                         , Left ("Min_cost"       , show $ metricDecoration ^. characterCostVector)
                         , Left ("Is_leaf"        , show $ metricDecoration ^. isLeaf             )
                         ]