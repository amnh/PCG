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

{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}

{-# LANGUAGE UndecidableInstances  #-}

module Bio.Character.Decoration.Dynamic.Internal
  ( DynamicDecorationDirectOptimization(..)
  , DynamicDecorationDirectOptimizationPostorderResult(..)
  , DynamicDecorationInitial(..)
  ) where

import Bio.Character.Decoration.Dynamic.Class
import Bio.Character.Decoration.Shared
import Bio.Character.Encodable
import Bio.Character.Exportable
import Control.DeepSeq
import Control.Lens
import Data.Bits
import Data.Hashable
import Data.MonoTraversable
import GHC.Generics
import Text.XML
import TextShow                               (Builder, TextShow (showb), toString, unlinesB, unwordsB)


-- |
-- An abstract direct optimization dynamic character decoration with a
-- polymorphic character type.
data  DynamicDecorationDirectOptimization d
    = DynamicDecorationDirectOptimization
    { dynamicDecorationDirectOptimizationCharacterCost            :: {-# UNPACK #-} !Word
    , dynamicDecorationDirectOptimizationCharacterLocalCost       :: {-# UNPACK #-} !Word
    , dynamicDecorationDirectOptimizationCharacterAverageLength   :: {-# UNPACK #-} !AverageLength
    , dynamicDecorationDirectOptimizationAlignmentContext         :: !d
    , dynamicDecorationDirectOptimizationImpliedAlignment         :: !d
    , dynamicDecorationDirectOptimizationSingleDisambiguation     :: !d
    }
    deriving anyclass (NFData)
    deriving stock    (Eq, Generic)


-- |
-- Represents the partial character decoration result of a post-order traversal.
data  DynamicDecorationDirectOptimizationPostorderResult d
    = DynamicDecorationDirectOptimizationPostorderResult
    { dynamicDecorationDirectOptimizationPostorderCharacterCost          :: {-# UNPACK #-} !Word
    , dynamicDecorationDirectOptimizationPostorderCharacterLocalCost     :: {-# UNPACK #-} !Word
    , dynamicDecorationDirectOptimizationPostorderCharacterAverageLength :: {-# UNPACK #-} !AverageLength
    , dynamicDecorationDirectOptimizationPostorderAlignmentContext       :: !d
    }
    deriving anyclass (NFData)
    deriving stock    (Eq, Generic)


-- |
-- An abstract initial dynamic character decoration with a polymorphic character
-- type.
data  DynamicDecorationInitial d
    = DynamicDecorationInitial
    { dynamicDecorationInitialEncodedField           :: !d
    , dynamicDecorationInitialCharacterAverageLength :: {-# UNPACK #-} !AverageLength
    }
    deriving anyclass (NFData)
    deriving stock    (Eq, Generic)


instance (EncodableDynamicCharacter d, ExportableBuffer (Subcomponent (Element d))) => DirectOptimizationDecoration (DynamicDecorationDirectOptimization d) d where


instance (EncodableDynamicCharacter d, ExportableBuffer (Subcomponent (Element d))) => DirectOptimizationPostorderDecoration (DynamicDecorationDirectOptimization d) d where


instance (EncodableDynamicCharacter d, ExportableBuffer (Subcomponent (Element d))) => DirectOptimizationPostorderDecoration (DynamicDecorationDirectOptimizationPostorderResult d) d where


instance (EncodableDynamicCharacter d, ExportableBuffer (Subcomponent (Element d))) => DynamicCharacterDecoration (DynamicDecorationInitial d) d where

--    toDynamicCharacterDecoration :: (x -> d) -> x -> (DynamicDecorationInitial d)
    toDynamicCharacterDecoration  g symbolSet =
        DynamicDecorationInitial
        { dynamicDecorationInitialEncodedField           = charValue
        , dynamicDecorationInitialCharacterAverageLength = toAverageLength . toEnum $ olength charValue
        }
      where
        charValue = g symbolSet


instance HasAverageLength (DynamicDecorationInitial d) AverageLength where

    averageLength = lens dynamicDecorationInitialCharacterAverageLength (\e x -> e { dynamicDecorationInitialCharacterAverageLength = x })


instance HasAverageLength (DynamicDecorationDirectOptimization d) AverageLength where

    averageLength = lens dynamicDecorationDirectOptimizationCharacterAverageLength (\e x -> e { dynamicDecorationDirectOptimizationCharacterAverageLength = x })


instance HasAverageLength (DynamicDecorationDirectOptimizationPostorderResult d) AverageLength where

    averageLength = lens dynamicDecorationDirectOptimizationPostorderCharacterAverageLength (\e x -> e { dynamicDecorationDirectOptimizationPostorderCharacterAverageLength = x })


instance HasCharacterCost (DynamicDecorationDirectOptimization d) Word where

    characterCost = lens dynamicDecorationDirectOptimizationCharacterCost (\e x -> e { dynamicDecorationDirectOptimizationCharacterCost = x })


instance HasCharacterCost (DynamicDecorationDirectOptimizationPostorderResult d) Word where

    characterCost = lens dynamicDecorationDirectOptimizationPostorderCharacterCost (\e x -> e { dynamicDecorationDirectOptimizationPostorderCharacterCost = x })


instance HasCharacterLocalCost (DynamicDecorationDirectOptimization d) Word where

    characterLocalCost = lens dynamicDecorationDirectOptimizationCharacterLocalCost (\e x -> e { dynamicDecorationDirectOptimizationCharacterLocalCost = x })


instance HasCharacterLocalCost (DynamicDecorationDirectOptimizationPostorderResult d) Word where

    characterLocalCost = lens dynamicDecorationDirectOptimizationPostorderCharacterLocalCost (\e x -> e { dynamicDecorationDirectOptimizationPostorderCharacterLocalCost = x })


instance HasEncoded (DynamicDecorationDirectOptimization d) d where

    encoded = lens dynamicDecorationDirectOptimizationImpliedAlignment (\e x -> e { dynamicDecorationDirectOptimizationImpliedAlignment = x })


instance HasEncoded (DynamicDecorationDirectOptimizationPostorderResult d) d where

    encoded = lens dynamicDecorationDirectOptimizationPostorderAlignmentContext (\e x -> e { dynamicDecorationDirectOptimizationPostorderAlignmentContext = x })


instance HasEncoded (DynamicDecorationInitial d) d where

    encoded = lens dynamicDecorationInitialEncodedField (\e x -> e { dynamicDecorationInitialEncodedField = x })


instance HasSingleDisambiguation (DynamicDecorationDirectOptimization d) d where

    singleDisambiguation = lens dynamicDecorationDirectOptimizationSingleDisambiguation (\e x -> e { dynamicDecorationDirectOptimizationSingleDisambiguation = x })


instance PossiblyMissingCharacter c => PossiblyMissingCharacter (DynamicDecorationInitial c) where

    isMissing = isMissing . (^. encoded)

    toMissing x = x & encoded %~ toMissing


instance PossiblyMissingCharacter c => PossiblyMissingCharacter (DynamicDecorationDirectOptimization c) where

    isMissing = isMissing . (^. encoded)

    toMissing x = x & encoded %~ toMissing


instance PossiblyMissingCharacter c => PossiblyMissingCharacter (DynamicDecorationDirectOptimizationPostorderResult c) where

    isMissing = isMissing . (^. encoded)

    toMissing x = x & encoded %~ toMissing


instance Hashable d => Hashable (DynamicDecorationDirectOptimization d) where

      hashWithSalt salt dec = foldr1 xor $
          [ hashWithSalt salt . dynamicDecorationDirectOptimizationCharacterCost
          , hashWithSalt salt . dynamicDecorationDirectOptimizationAlignmentContext
          , hashWithSalt salt . dynamicDecorationDirectOptimizationImpliedAlignment
          , hashWithSalt salt . dynamicDecorationDirectOptimizationSingleDisambiguation
          ] <*> [dec]


instance Hashable d => Hashable (DynamicDecorationDirectOptimizationPostorderResult d) where

      hashWithSalt salt dec = foldr1 xor $
                              [ hashWithSalt salt . dynamicDecorationDirectOptimizationPostorderCharacterCost
                              , hashWithSalt salt . dynamicDecorationDirectOptimizationPostorderAlignmentContext
                              ] <*> [dec]


instance Hashable d => Hashable (DynamicDecorationInitial d) where

    hashWithSalt salt = hashWithSalt salt . dynamicDecorationInitialEncodedField


instance HasImpliedAlignment (DynamicDecorationDirectOptimization d) d where

    impliedAlignment = lens dynamicDecorationDirectOptimizationImpliedAlignment (\e x -> e { dynamicDecorationDirectOptimizationImpliedAlignment = x })


instance HasAlignmentContext (DynamicDecorationDirectOptimization d) d where

    alignmentContext = lens dynamicDecorationDirectOptimizationAlignmentContext (\e x -> e { dynamicDecorationDirectOptimizationAlignmentContext = x })


instance HasAlignmentContext (DynamicDecorationDirectOptimizationPostorderResult d) d where

    alignmentContext = lens dynamicDecorationDirectOptimizationPostorderAlignmentContext (\e x -> e { dynamicDecorationDirectOptimizationPostorderAlignmentContext = x })


instance (EncodableDynamicCharacter d, ExportableBuffer (Subcomponent (Element d))) => PostorderExtensionDirectOptimizationDecoration (DynamicDecorationDirectOptimization d) d where

    extendPostorderToDirectOptimization subDecoration single ia =
        DynamicDecorationDirectOptimization
        { dynamicDecorationDirectOptimizationCharacterCost            = subDecoration ^. characterCost
        , dynamicDecorationDirectOptimizationCharacterLocalCost       = subDecoration ^. characterLocalCost
        , dynamicDecorationDirectOptimizationCharacterAverageLength   = subDecoration ^. averageLength
        , dynamicDecorationDirectOptimizationAlignmentContext         = subDecoration ^. alignmentContext
        , dynamicDecorationDirectOptimizationImpliedAlignment         = ia
        , dynamicDecorationDirectOptimizationSingleDisambiguation     = single
        }


instance (EncodableStream d, TextShow d) => Show (DynamicDecorationDirectOptimization d) where

    show = toString . showb


instance (EncodableStream d, TextShow d) => Show (DynamicDecorationDirectOptimizationPostorderResult d) where

    show = toString . showb 


instance ( EncodableStreamElement (Element d)
         , MonoFoldable d
         , PossiblyMissingCharacter d
         , Show (Element d)
         ) => Show (DynamicDecorationInitial d) where

    show dec
      | isMissing character = "<Missing>"
      | otherwise           = ofoldMap show character
      where
        character = dec ^. encoded


instance (EncodableStream d, TextShow d) => TextShow (DynamicDecorationDirectOptimization d) where

    showb dec = unlinesB . (shownCost:) $ f <$> pairs
      where
        shownCost = renderCostB dec

        f (prefix, accessor) = prefix <> showb (dec ^. accessor)

        pairs =
            [ ("Single Disambiguation: ", singleDisambiguation)
            , ("Alignemnt Context    : ", alignmentContext    )
            , ("Implied Alignment    : ", impliedAlignment    )
            ]


instance (EncodableStream d, TextShow d) => TextShow (DynamicDecorationDirectOptimizationPostorderResult d) where

    showb dec = unlinesB . (shownCost:) $ f <$> pairs
      where
        shownCost = renderCostB dec

        f (prefix, accessor) = prefix <> showb (dec ^. accessor)

        pairs =
          [ ("Alignment Context   : ", alignmentContext)
          ]


instance (EncodableDynamicCharacter d, ExportableBuffer (Subcomponent (Element d))) => SimpleDynamicDecoration (DynamicDecorationDirectOptimization d) d where


instance (EncodableDynamicCharacter d, ExportableBuffer (Subcomponent (Element d))) => SimpleDynamicDecoration (DynamicDecorationDirectOptimizationPostorderResult d) d where


instance (EncodableDynamicCharacter d, ExportableBuffer (Subcomponent (Element d))) => SimpleDynamicDecoration (DynamicDecorationInitial d) d where


instance (EncodableDynamicCharacter d, ExportableBuffer (Subcomponent (Element d))) => SimpleDynamicExtensionPostorderDecoration (DynamicDecorationDirectOptimizationPostorderResult d) d where

    extendDynamicToPostorder _ localCost totalCost subtreeAvgLength subtreeAlignment =
        DynamicDecorationDirectOptimizationPostorderResult
        { dynamicDecorationDirectOptimizationPostorderCharacterCost          = totalCost
        , dynamicDecorationDirectOptimizationPostorderCharacterLocalCost     = localCost
        , dynamicDecorationDirectOptimizationPostorderCharacterAverageLength = subtreeAvgLength
        , dynamicDecorationDirectOptimizationPostorderAlignmentContext       = subtreeAlignment
        }


instance Show d => ToXML (DynamicDecorationDirectOptimization d) where

    toXML decoration = xmlElement "Dynamic_DO_pre-order_decoration_result" attributes contents
        where
            attributes = []
            contents   = [ Left ("Character_cost"           , show $ decoration ^. characterCost      )
                         , Left ("Local_cost"               , show $ decoration ^. characterLocalCost )
                         , Left ("Alignment_Context"        , show $ decoration ^. alignmentContext  )
                         , Left ("Implied_Alignemnt"        , show $ decoration ^. alignmentContext  )
                         ]


instance Show d => ToXML (DynamicDecorationDirectOptimizationPostorderResult d) where

    toXML decoration = xmlElement "Dynamic_DO_post-order_decoration_result" attributes contents
        where
            attributes = []
            contents   = [ Left ("Character_cost"           , show $ decoration ^. characterCost      )
                         , Left ("Local_cost"               , show $ decoration ^. characterLocalCost )
                         , Left ("Alignment_Context"        , show $ decoration ^. alignmentContext  )
                         ]


renderCostB
  :: ( HasCharacterCost s a
     , HasCharacterLocalCost s b
     , TextShow a
     , TextShow b
     )
  => s
  -> Builder
renderCostB dec = unwordsB
      [ "Cost                 :"
      , showb (dec ^. characterCost)
      , "{"
      , showb (dec ^. characterLocalCost)
      , "}"
      ]
