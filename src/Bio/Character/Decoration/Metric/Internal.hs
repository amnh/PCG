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

{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}

module Bio.Character.Decoration.Metric.Internal where


import Bio.Character.Decoration.Discrete
import Bio.Character.Decoration.Metric.Class
import Bio.Character.Decoration.Shared
import Bio.Character.Encodable
import Control.DeepSeq
import Control.Lens
import GHC.Generics
import Numeric.Extended.Natural
import Text.XML
import TextShow                              (TextShow (showb))


-- |
-- An abstract initial dynamic character decoration with a polymorphic character
-- type.
newtype MetricDecorationInitial c
    = MetricDecorationInitial
    { metricDecorationInitialCharacter :: c
    } deriving (Generic)


-- |
-- A concrete type representing the results of performing Sankoff's algorithm.
data SankoffOptimizationDecoration c
   = SankoffOptimizationDecoration
                                     -- tuple of (a,a) where a is a per-parent-state list of
                                     -- lists of child states that contributed to the minimum
                                     -- cost of that state
   { sankoffMinStateTuple         :: {-# UNPACK #-} !([StateContributionList], [StateContributionList])
   , sankoffMinCostVector         :: ![ExtendedNatural]   -- minimum total cost per state (left + right)
   , sankoffMinCost               :: {-# UNPACK #-} !Word -- overall minimum cost for all states
   , sankoffPreliminaryExtraCosts :: ![ExtendedNatural]   -- list of preliminary per-character-state extra costs
                                                          -- for the node; these store only the costs for assigning
                                                          -- this state to THIS node, rather than the accumulated
                                                          -- extra costs down through the tree when this assignment
                                                          -- is chosen
   , sankoffFinalExtraCosts       :: ![ExtendedNatural]   -- list of final extra costs for the node, so the sum of
                                                          -- ALL of the extra costs, on the whole tree, for this
                                                          -- assignment
   , sankoffBeta                  :: ![ExtendedNatural]   -- this is Goloboff's beta, where
                                                          -- beta_(s,n) = min[t_(s,x) + prelimExtraCost_(x,n)]
                                                          -- where t_(s,x) is the transition cost from state s to x
   , sankoffCharacterField        :: c                    -- Bit Vector version of median character
   , sankoffIsLeaf                :: !Bool
   } deriving (Generic)


-- | A list of states on the child that contribute to the lowest score on each state in the parent
-- Used to simplify? SankoffOptimizationDecoration
type StateContributionList = [Word]


-- | (✔)
instance NFData c => NFData (MetricDecorationInitial c)


-- | (✔)
instance NFData c => NFData (SankoffOptimizationDecoration c)


-- | (✔)
instance EncodableStaticCharacter c => DiscreteCharacterDecoration (MetricDecorationInitial c) c where


-- | (✔)
instance EncodableStaticCharacter c => DiscreteCharacterDecoration (SankoffOptimizationDecoration c) c where


-- | (✔)
instance EncodableStaticCharacter c => DiscreteExtensionSankoffDecoration (SankoffOptimizationDecoration c) c where

    extendDiscreteToSankoff _subDecoration costVector prelimExtras finalExtras inputBeta childMinStates cost newMedian leaf =

        SankoffOptimizationDecoration
        { sankoffMinStateTuple         = childMinStates
        , sankoffMinCostVector         = costVector
        , sankoffPreliminaryExtraCosts = prelimExtras
        , sankoffFinalExtraCosts       = finalExtras
        , sankoffBeta                  = inputBeta
        , sankoffMinCost               = cost
        , sankoffCharacterField        = newMedian
        , sankoffIsLeaf                = leaf
        }


-- | (✔)
instance HasBeta (SankoffOptimizationDecoration c) [ExtendedNatural] where

    beta = lens sankoffBeta (\e x -> e { sankoffBeta = x })


-- | (✔)
instance HasCharacterCost (SankoffOptimizationDecoration c) Word where

    characterCost = lens sankoffMinCost (\e x -> e { sankoffMinCost = x })


-- | (✔)
instance HasCharacterCostVector (SankoffOptimizationDecoration c) [ExtendedNatural] where

    characterCostVector = lens sankoffMinCostVector (\e x -> e { sankoffMinCostVector = x })


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
instance EncodableStaticCharacter c => MetricCharacterDecoration (MetricDecorationInitial c) c where


-- | (✔)
instance EncodableStaticCharacter c => MetricCharacterDecoration (SankoffOptimizationDecoration c) c where


-- | (✔)
instance EncodableStaticCharacter c => SankoffDecoration (SankoffOptimizationDecoration c) c where


-- | (✔)
instance Show c => Show (SankoffOptimizationDecoration c) where

    show x = show $ x ^. discreteCharacter


-- | (✔)
instance TextShow c => TextShow (SankoffOptimizationDecoration c) where

    showb x = showb $ x ^. discreteCharacter


-- | (✔)
instance EncodableStaticCharacter c => SimpleDiscreteCharacterDecoration (MetricDecorationInitial c) c where

    toDiscreteCharacterDecoration g symbolSet =
        MetricDecorationInitial
        { metricDecorationInitialCharacter = g symbolSet
        }


-- | (✔)
instance ToXML (SankoffOptimizationDecoration c) where

    toXML metricDecoration = xmlElement "Sankoff_optimization_decoration" attributes contents
      where
        attributes = []
        contents   = [ Left ("Min_cost_vector", show $ metricDecoration ^. characterCost      )
                     , Left ("Min_cost"       , show $ metricDecoration ^. characterCostVector)
                     , Left ("Is_leaf"        , show $ metricDecoration ^. isLeaf             )
                     ]
