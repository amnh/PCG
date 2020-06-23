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

{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}

module Bio.Character.Decoration.Fitch.Internal where

import Bio.Character.Decoration.Discrete
import Bio.Character.Decoration.Fitch.Class
import Bio.Character.Decoration.Shared
import Bio.Character.Encodable
import Control.DeepSeq
import Control.Lens
import Data.Bits
import GHC.Generics
import Text.XML
import TextShow                             (TextShow (showb), unlinesB)


-- |
-- An abstract initial dynamic character decoration with a polymorphic character
-- type.
data  FitchOptimizationDecoration f
    = FitchOptimizationDecoration
    { fitchMinCost           :: {-# UNPACK #-} !Word   -- Cost of the subtree
    , fitchPreliminaryMedian :: !f                     -- Held here until final state is
                                                      --     determined and we can assign that
                                                      --     into discreteCharacter
    , fitchFinalMedian       :: !f                     -- Eventually gets assigned to discreteCharacter
    , fitchChildMedians      :: {-# UNPACK #-} !(f, f) -- (left, right) so that we can do post order
                                                      --     pass with all of Fitch's rules
    , fitchIsLeaf            :: !Bool                  -- need this in preorder
    , fitchCharacterField    :: !f
    }
    deriving stock    (Generic)
    deriving anyclass (NFData)


instance HasDiscreteCharacter (FitchOptimizationDecoration f) f where

    discreteCharacter = lens fitchCharacterField (\e x -> e { fitchCharacterField = x })


instance HasIsLeaf (FitchOptimizationDecoration f) Bool where

    isLeaf = lens fitchIsLeaf (\e x -> e { fitchIsLeaf = x })


instance HasCharacterCost (FitchOptimizationDecoration f) Word where

    characterCost = lens fitchMinCost (\e x -> e { fitchMinCost = x })


instance HasPreliminaryMedian (FitchOptimizationDecoration f) f where

    preliminaryMedian = lens fitchPreliminaryMedian (\e x -> e { fitchPreliminaryMedian = x })


instance HasChildMedians (FitchOptimizationDecoration f) ( f, f ) where

    childMedians = lens fitchChildMedians (\e x -> e { fitchChildMedians = x })


instance HasFinalMedian (FitchOptimizationDecoration f) f where

    finalMedian = lens fitchFinalMedian (\e x -> e { fitchFinalMedian = x })


instance EncodableStaticCharacter f => DiscreteCharacterDecoration (FitchOptimizationDecoration f) f where


instance EncodableStaticCharacter f => FitchCharacterDecoration (FitchOptimizationDecoration f) f where


instance EncodableStaticCharacter f => FitchDecoration (FitchOptimizationDecoration f) f where


instance EncodableStaticCharacter f => DiscreteExtensionFitchDecoration (FitchOptimizationDecoration f) f where

    extendDiscreteToFitch subDecoration cost prelimMedian finMedian childMedianTup isLeafVal =

        FitchOptimizationDecoration
        { fitchChildMedians      = childMedianTup
        , fitchIsLeaf            = isLeafVal
        , fitchMinCost           = cost
        , fitchPreliminaryMedian = prelimMedian
        , fitchFinalMedian       = finMedian
        , fitchCharacterField    = subDecoration ^. discreteCharacter
        }


instance (Bits c, Show c) => Show (FitchOptimizationDecoration c) where

    show c = unlines
        [ "Discrete Character : "    <> showStatic (fitchCharacterField     c)
        , "Preliminary Median : "    <> showStatic (fitchPreliminaryMedian  c)
        , "Final       Median : "    <> showStatic (fitchFinalMedian        c)
        ]
      where
        showStatic x
          | x == zeroBits = "<Empty Character>"
          | otherwise     = show x


instance (Bits c, TextShow c) => TextShow (FitchOptimizationDecoration c) where

    showb c = unlinesB
        [ "Discrete Character : "    <> showStatic (fitchCharacterField     c)
        , "Preliminary Median : "    <> showStatic (fitchPreliminaryMedian  c)
        , "Final       Median : "    <> showStatic (fitchFinalMedian        c)
        ]
      where
        showStatic x
          | x == zeroBits = "<Empty Character>"
          | otherwise     = showb x


instance (Show f) => ToXML (FitchOptimizationDecoration f) where

    toXML decoration = xmlElement "Fitch_decoration" attributes contents
        where
            attributes = []
            contents   = [ Left ("Min_cost",           show $ decoration ^. characterCost    )
                         , Left ("Preliminary_median", show $ decoration ^. preliminaryMedian)
                         , Left ("Final_median",       show $ decoration ^. finalMedian      )
                         , Left ("Is_a_leaf",          show $ decoration ^. isLeaf           )
                         ]



