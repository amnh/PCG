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

{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

{-# LANGUAGE UndecidableInstances  #-}

module Bio.Character.Decoration.Dynamic.Internal
  ( DynamicDecorationDirectOptimization(..)
  , DynamicDecorationDirectOptimizationPostorderResult(..)
  , DynamicDecorationImpliedAlignment(..)
  , DynamicDecorationInitial(..)
  ) where

import Bio.Character.Decoration.Dynamic.Class
import Bio.Character.Decoration.Shared
import Bio.Character.Encodable
import Control.DeepSeq
import Control.Lens
import Data.Bits
import Data.Hashable
import Data.MonoTraversable
import GHC.Generics
import Text.XML


-- |
-- An abstract direct optimization dynamic character decoration with a
-- polymorphic character type.
data DynamicDecorationDirectOptimization d
   = DynamicDecorationDirectOptimization
   { dynamicDecorationDirectOptimizationCharacterCost            :: {-# UNPACK #-} !Word
   , dynamicDecorationDirectOptimizationCharacterLocalCost       :: {-# UNPACK #-} !Word
   , dynamicDecorationDirectOptimizationCharacterAverageLength   :: {-# UNPACK #-} !AverageLength
   , dynamicDecorationDirectOptimizationSingleDisambiguation     :: !d
   , dynamicDecorationDirectOptimizationEncodedField             :: !d
   , dynamicDecorationDirectOptimizationFinalGappedField         :: !d
   , dynamicDecorationDirectOptimizationFinalUngappedField       :: !d
   , dynamicDecorationDirectOptimizationPreliminaryGappedField   :: !d
   , dynamicDecorationDirectOptimizationPreliminaryUngappedField :: !d
   , dynamicDecorationDirectOptimizationLeftAlignmentField       :: !d
   , dynamicDecorationDirectOptimizationRightAlignmentField      :: !d
   } deriving (Eq, Generic)


-- |
-- Represents the partial character decoration result of a post-order traversal.
data DynamicDecorationDirectOptimizationPostorderResult d
   = DynamicDecorationDirectOptimizationPostorderResult
   { dynamicDecorationDirectOptimizationPostorderCharacterCost            :: {-# UNPACK #-} !Word
   , dynamicDecorationDirectOptimizationPostorderCharacterLocalCost       :: {-# UNPACK #-} !Word
   , dynamicDecorationDirectOptimizationPostorderCharacterAverageLength   :: {-# UNPACK #-} !AverageLength
   , dynamicDecorationDirectOptimizationPostorderEncodedField             :: !d
   , dynamicDecorationDirectOptimizationPostorderPreliminaryGappedField   :: !d
   , dynamicDecorationDirectOptimizationPostorderPreliminaryUngappedField :: !d
   , dynamicDecorationDirectOptimizationPostorderLeftAlignmentField       :: !d
   , dynamicDecorationDirectOptimizationPostorderRightAlignmentField      :: !d
   } deriving (Eq, Generic)


-- |
-- An abstract implied alignment dynamic character decoration with a polymorphic
-- character type.
data DynamicDecorationImpliedAlignment d
   = DynamicDecorationImpliedAlignment
   { dynamicDecorationImpliedAlignmentCharacterCost            :: {-# UNPACK #-} !Word
   , dynamicDecorationImpliedAlignmentCharacterLocalCost       :: {-# UNPACK #-} !Word
   , dynamicDecorationImpliedAlignmentCharacterAverageLength   :: {-# UNPACK #-} !AverageLength
   , dynamicDecorationImpliedAlignmentEncodedField             :: !d
   , dynamicDecorationImpliedAlignmentFinalGappedField         :: !d
   , dynamicDecorationImpliedAlignmentFinalUngappedField       :: !d
   , dynamicDecorationImpliedAlignmentPreliminaryGappedField   :: !d
   , dynamicDecorationImpliedAlignmentPreliminaryUngappedField :: !d
   , dynamicDecorationImpliedAlignmentLeftAlignmentField       :: !d
   , dynamicDecorationImpliedAlignmentRightAlignmentField      :: !d
   , dynamicDecorationImpliedAlignmentImpliedAlignmentField    :: !d
   } deriving (Eq, Generic)


-- |
-- An abstract initial dynamic character decoration with a polymorphic character
-- type.
data DynamicDecorationInitial d
   = DynamicDecorationInitial
   { dynamicDecorationInitialEncodedField           :: !d
   , dynamicDecorationInitialCharacterAverageLength :: {-# UNPACK #-} !AverageLength
   } deriving (Eq, Generic)


-- | (✔)
instance NFData d => NFData (DynamicDecorationInitial d)


-- | (✔)
instance NFData d => NFData (DynamicDecorationDirectOptimization d)


-- | (✔)
instance NFData d => NFData (DynamicDecorationDirectOptimizationPostorderResult d)


-- | (✔)
instance NFData d => NFData (DynamicDecorationImpliedAlignment d)


-- | (✔)
instance EncodableDynamicCharacter d => DirectOptimizationDecoration (DynamicDecorationDirectOptimization d) d where


-- | (✔)
instance EncodableDynamicCharacter d => DirectOptimizationDecoration (DynamicDecorationImpliedAlignment d) d where


-- | (✔)
instance EncodableDynamicCharacter d => DirectOptimizationPostorderDecoration (DynamicDecorationDirectOptimization d) d where


-- | (✔)
instance EncodableDynamicCharacter d => DirectOptimizationPostorderDecoration (DynamicDecorationDirectOptimizationPostorderResult d) d where


-- | (✔)
instance EncodableDynamicCharacter d => DirectOptimizationPostorderDecoration (DynamicDecorationImpliedAlignment d) d where


-- | (✔)
instance (EncodableDynamicCharacter d) => DynamicCharacterDecoration (DynamicDecorationInitial d) d where

--    toDynamicCharacterDecoration :: CharacterName -> Double -> Alphabet String -> TCM -> (x -> a) -> x -> s
    toDynamicCharacterDecoration g symbolSet =
        DynamicDecorationInitial
        { dynamicDecorationInitialEncodedField           = charValue
        , dynamicDecorationInitialCharacterAverageLength = toAverageLength . toEnum $ olength charValue
        }
      where
        charValue = g symbolSet


-- | (✔)
instance HasAverageLength (DynamicDecorationInitial d) AverageLength where

    averageLength = lens dynamicDecorationInitialCharacterAverageLength (\e x -> e { dynamicDecorationInitialCharacterAverageLength = x })


-- | (✔)
instance HasAverageLength (DynamicDecorationDirectOptimization d) AverageLength where

    averageLength = lens dynamicDecorationDirectOptimizationCharacterAverageLength (\e x -> e { dynamicDecorationDirectOptimizationCharacterAverageLength = x })


-- | (✔)
instance HasAverageLength (DynamicDecorationImpliedAlignment d) AverageLength where

    averageLength = lens dynamicDecorationImpliedAlignmentCharacterAverageLength (\e x -> e { dynamicDecorationImpliedAlignmentCharacterAverageLength = x })


-- | (✔)
instance HasAverageLength (DynamicDecorationDirectOptimizationPostorderResult d) AverageLength where

    averageLength = lens dynamicDecorationDirectOptimizationPostorderCharacterAverageLength (\e x -> e { dynamicDecorationDirectOptimizationPostorderCharacterAverageLength = x })


-- | (✔)
instance HasCharacterCost (DynamicDecorationImpliedAlignment d) Word where

    characterCost = lens dynamicDecorationImpliedAlignmentCharacterCost (\e x -> e { dynamicDecorationImpliedAlignmentCharacterCost = x })


-- | (✔)
instance HasCharacterCost (DynamicDecorationDirectOptimization d) Word where

    characterCost = lens dynamicDecorationDirectOptimizationCharacterCost (\e x -> e { dynamicDecorationDirectOptimizationCharacterCost = x })


-- | (✔)
instance HasCharacterCost (DynamicDecorationDirectOptimizationPostorderResult d) Word where

    characterCost = lens dynamicDecorationDirectOptimizationPostorderCharacterCost (\e x -> e { dynamicDecorationDirectOptimizationPostorderCharacterCost = x })


-- | (✔)
instance HasCharacterLocalCost (DynamicDecorationImpliedAlignment d) Word where

    characterLocalCost = lens dynamicDecorationImpliedAlignmentCharacterLocalCost (\e x -> e { dynamicDecorationImpliedAlignmentCharacterLocalCost = x })


-- | (✔)
instance HasCharacterLocalCost (DynamicDecorationDirectOptimization d) Word where

    characterLocalCost = lens dynamicDecorationDirectOptimizationCharacterLocalCost (\e x -> e { dynamicDecorationDirectOptimizationCharacterLocalCost = x })


-- | (✔)
instance HasCharacterLocalCost (DynamicDecorationDirectOptimizationPostorderResult d) Word where

    characterLocalCost = lens dynamicDecorationDirectOptimizationPostorderCharacterLocalCost (\e x -> e { dynamicDecorationDirectOptimizationPostorderCharacterLocalCost = x })


-- | (✔)
instance HasEncoded (DynamicDecorationImpliedAlignment d) d where

    encoded = lens dynamicDecorationImpliedAlignmentEncodedField (\e x -> e { dynamicDecorationImpliedAlignmentEncodedField = x })


-- | (✔)
instance HasEncoded (DynamicDecorationDirectOptimization d) d where

    encoded = lens dynamicDecorationDirectOptimizationEncodedField (\e x -> e { dynamicDecorationDirectOptimizationEncodedField = x })


-- | (✔)
instance HasEncoded (DynamicDecorationDirectOptimizationPostorderResult d) d where

    encoded = lens dynamicDecorationDirectOptimizationPostorderEncodedField (\e x -> e { dynamicDecorationDirectOptimizationPostorderEncodedField = x })


-- | (✔)
instance HasEncoded (DynamicDecorationInitial d) d where

    encoded = lens dynamicDecorationInitialEncodedField (\e x -> e { dynamicDecorationInitialEncodedField = x })


-- | (✔)
instance HasSingleDisambiguation (DynamicDecorationDirectOptimization d) d where

    singleDisambiguation = lens dynamicDecorationDirectOptimizationSingleDisambiguation (\e x -> e { dynamicDecorationDirectOptimizationSingleDisambiguation = x })


-- | (✔)
instance PossiblyMissingCharacter c => PossiblyMissingCharacter (DynamicDecorationInitial c) where

    isMissing = isMissing . (^. encoded)

    toMissing x = x & encoded %~ toMissing


-- | (✔)
instance PossiblyMissingCharacter c => PossiblyMissingCharacter (DynamicDecorationDirectOptimization c) where

    isMissing = isMissing . (^. encoded)

    toMissing x = x & encoded %~ toMissing


-- | (✔)
instance PossiblyMissingCharacter c => PossiblyMissingCharacter (DynamicDecorationDirectOptimizationPostorderResult c) where

    isMissing = isMissing . (^. encoded)

    toMissing x = x & encoded %~ toMissing


-- | (✔)
instance PossiblyMissingCharacter c => PossiblyMissingCharacter (DynamicDecorationImpliedAlignment c) where

    isMissing = isMissing . (^. encoded)

    toMissing x = x & encoded %~ toMissing


-- | (✔)
instance HasFinalGapped (DynamicDecorationImpliedAlignment d) d where

    finalGapped = lens dynamicDecorationImpliedAlignmentFinalGappedField (\e x -> e { dynamicDecorationImpliedAlignmentFinalGappedField = x })


-- | (✔)
instance HasFinalGapped (DynamicDecorationDirectOptimization d) d where

    finalGapped = lens dynamicDecorationDirectOptimizationFinalGappedField (\e x -> e { dynamicDecorationDirectOptimizationFinalGappedField = x })


-- | (✔)
instance HasFinalUngapped (DynamicDecorationImpliedAlignment d) d where

    finalUngapped = lens dynamicDecorationImpliedAlignmentFinalUngappedField (\e x -> e { dynamicDecorationImpliedAlignmentFinalUngappedField = x })


-- | (✔)
instance HasFinalUngapped (DynamicDecorationDirectOptimization d) d where

    finalUngapped = lens dynamicDecorationDirectOptimizationFinalUngappedField (\e x -> e { dynamicDecorationDirectOptimizationFinalUngappedField = x })


-- | (✔)
instance Hashable d => Hashable (DynamicDecorationDirectOptimization d) where

      hashWithSalt salt dec = foldr1 xor $
          [ hashWithSalt salt . dynamicDecorationDirectOptimizationCharacterCost
          , hashWithSalt salt . dynamicDecorationDirectOptimizationEncodedField
          , hashWithSalt salt . dynamicDecorationDirectOptimizationFinalGappedField
          , hashWithSalt salt . dynamicDecorationDirectOptimizationFinalUngappedField
          , hashWithSalt salt . dynamicDecorationDirectOptimizationPreliminaryGappedField
          , hashWithSalt salt . dynamicDecorationDirectOptimizationPreliminaryUngappedField
          , hashWithSalt salt . dynamicDecorationDirectOptimizationLeftAlignmentField
          , hashWithSalt salt . dynamicDecorationDirectOptimizationRightAlignmentField
          ] <*> [dec]


-- | (✔)
instance Hashable d => Hashable (DynamicDecorationDirectOptimizationPostorderResult d) where

      hashWithSalt salt dec = foldr1 xor $
                              [ hashWithSalt salt . dynamicDecorationDirectOptimizationPostorderCharacterCost
                              , hashWithSalt salt . dynamicDecorationDirectOptimizationPostorderEncodedField
                              , hashWithSalt salt . dynamicDecorationDirectOptimizationPostorderPreliminaryGappedField
                              , hashWithSalt salt . dynamicDecorationDirectOptimizationPostorderPreliminaryUngappedField
                              , hashWithSalt salt . dynamicDecorationDirectOptimizationPostorderLeftAlignmentField
                              , hashWithSalt salt . dynamicDecorationDirectOptimizationPostorderRightAlignmentField
                              ] <*> [dec]


-- | (✔)
instance Hashable d => Hashable (DynamicDecorationInitial d) where

    hashWithSalt salt = hashWithSalt salt . dynamicDecorationInitialEncodedField


-- | (✔)
instance HasImpliedAlignment (DynamicDecorationImpliedAlignment d) d where

    impliedAlignment = lens dynamicDecorationImpliedAlignmentImpliedAlignmentField (\e x -> e { dynamicDecorationImpliedAlignmentImpliedAlignmentField = x })


-- | (✔)
instance HasLeftAlignment (DynamicDecorationImpliedAlignment d) d where

    leftAlignment = lens dynamicDecorationImpliedAlignmentLeftAlignmentField (\e x -> e { dynamicDecorationImpliedAlignmentLeftAlignmentField = x })


-- | (✔)
instance HasLeftAlignment (DynamicDecorationDirectOptimization d) d where

    leftAlignment = lens dynamicDecorationDirectOptimizationLeftAlignmentField (\e x -> e { dynamicDecorationDirectOptimizationLeftAlignmentField = x })


-- | (✔)
instance HasLeftAlignment (DynamicDecorationDirectOptimizationPostorderResult d) d where

    leftAlignment = lens dynamicDecorationDirectOptimizationPostorderLeftAlignmentField (\e x -> e { dynamicDecorationDirectOptimizationPostorderLeftAlignmentField = x })


-- | (✔)
instance HasPreliminaryGapped (DynamicDecorationImpliedAlignment d) d where

    preliminaryGapped = lens dynamicDecorationImpliedAlignmentPreliminaryGappedField (\e x -> e { dynamicDecorationImpliedAlignmentPreliminaryGappedField = x })


-- | (✔)
instance HasPreliminaryGapped (DynamicDecorationDirectOptimization d) d where

    preliminaryGapped = lens dynamicDecorationDirectOptimizationPreliminaryGappedField (\e x -> e { dynamicDecorationDirectOptimizationPreliminaryGappedField = x })


-- | (✔)
instance HasPreliminaryGapped (DynamicDecorationDirectOptimizationPostorderResult d) d where

    preliminaryGapped = lens dynamicDecorationDirectOptimizationPostorderPreliminaryGappedField (\e x -> e { dynamicDecorationDirectOptimizationPostorderPreliminaryGappedField = x })


-- | (✔)
instance HasPreliminaryUngapped (DynamicDecorationImpliedAlignment d) d where

    preliminaryUngapped = lens dynamicDecorationImpliedAlignmentPreliminaryUngappedField (\e x -> e { dynamicDecorationImpliedAlignmentPreliminaryUngappedField = x })


-- | (✔)
instance HasPreliminaryUngapped (DynamicDecorationDirectOptimization d) d where

    preliminaryUngapped = lens dynamicDecorationDirectOptimizationPreliminaryUngappedField (\e x -> e { dynamicDecorationDirectOptimizationPreliminaryUngappedField = x })


-- | (✔)
instance HasPreliminaryUngapped (DynamicDecorationDirectOptimizationPostorderResult d) d where

    preliminaryUngapped = lens dynamicDecorationDirectOptimizationPostorderPreliminaryUngappedField (\e x -> e { dynamicDecorationDirectOptimizationPostorderPreliminaryUngappedField = x })


-- | (✔)
instance HasRightAlignment (DynamicDecorationImpliedAlignment d) d where

    rightAlignment = lens dynamicDecorationImpliedAlignmentLeftAlignmentField (\e x -> e { dynamicDecorationImpliedAlignmentLeftAlignmentField = x })


-- | (✔)
instance HasRightAlignment (DynamicDecorationDirectOptimization d) d where

    rightAlignment = lens dynamicDecorationDirectOptimizationRightAlignmentField (\e x -> e { dynamicDecorationDirectOptimizationRightAlignmentField = x })


-- | (✔)
instance HasRightAlignment (DynamicDecorationDirectOptimizationPostorderResult d) d where

    rightAlignment = lens dynamicDecorationDirectOptimizationPostorderRightAlignmentField (\e x -> e { dynamicDecorationDirectOptimizationPostorderRightAlignmentField = x })


-- | (✔)
instance EncodableDynamicCharacter d => ImpliedAlignmentDecoration   (DynamicDecorationImpliedAlignment d) d where


-- | (✔)
instance EncodableDynamicCharacter d => PostorderExtensionDirectOptimizationDecoration (DynamicDecorationDirectOptimization d) d where

    extendPostorderToDirectOptimization subDecoration ungapped gapped single =
        DynamicDecorationDirectOptimization
        { dynamicDecorationDirectOptimizationCharacterCost            = subDecoration ^. characterCost
        , dynamicDecorationDirectOptimizationCharacterLocalCost       = subDecoration ^. characterLocalCost
        , dynamicDecorationDirectOptimizationCharacterAverageLength   = subDecoration ^. averageLength
        , dynamicDecorationDirectOptimizationEncodedField             = subDecoration ^. encoded
        , dynamicDecorationDirectOptimizationFinalGappedField         = gapped
        , dynamicDecorationDirectOptimizationFinalUngappedField       = ungapped
        , dynamicDecorationDirectOptimizationSingleDisambiguation     = single
        , dynamicDecorationDirectOptimizationPreliminaryGappedField   = subDecoration ^. preliminaryGapped
        , dynamicDecorationDirectOptimizationPreliminaryUngappedField = subDecoration ^. preliminaryUngapped
        , dynamicDecorationDirectOptimizationLeftAlignmentField       = subDecoration ^. leftAlignment
        , dynamicDecorationDirectOptimizationRightAlignmentField      = subDecoration ^. rightAlignment
        }


-- | (✔)
instance (EncodableStream d, Show d) => Show (DynamicDecorationDirectOptimization d) where

    show dec = unlines . (shownCost:) $ f <$> pairs
      where
        shownCost = renderCost dec

        f (prefix, accessor) = prefix <> show (dec ^. accessor)

        pairs =
            [ ("Original Encoding    : ", encoded             )
            , ("Single Disambiguation: ", singleDisambiguation)
            , ("Final        Ungapped: ", finalUngapped       )
            , ("Final          Gapped: ", finalGapped         )
            , ("Preliminary  Ungapped: ", preliminaryUngapped )
            , ("Preliminary    Gapped: ", preliminaryGapped   )
            , ("Left  Alignment      : ", leftAlignment       )
            , ("Right Alignment      : ", rightAlignment      )
            ]


-- | (✔)
instance (EncodableStream d, Show d) => Show (DynamicDecorationDirectOptimizationPostorderResult d) where

    show dec = unlines . (shownCost:) $ f <$> pairs
      where
        shownCost = renderCost dec

        f (prefix, accessor) = prefix <> show (dec ^. accessor)

        pairs =
          [ ("Original Encoding   : ", encoded            )
          , ("Preliminary Ungapped: ", preliminaryUngapped)
          , ("Preliminary   Gapped: ", preliminaryGapped  )
          , ("Left  Alignment     : ", leftAlignment      )
          , ("Right Alignment     : ", rightAlignment     )
          ]


-- | (✔)
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


-- | (✔)
instance EncodableDynamicCharacter d => SimpleDynamicDecoration (DynamicDecorationDirectOptimization d) d where


-- | (✔)
instance EncodableDynamicCharacter d => SimpleDynamicDecoration (DynamicDecorationDirectOptimizationPostorderResult d) d where


-- | (✔)
instance EncodableDynamicCharacter d => SimpleDynamicDecoration (DynamicDecorationImpliedAlignment d) d where


-- | (✔)
instance EncodableDynamicCharacter d => SimpleDynamicDecoration (DynamicDecorationInitial d) d where


-- | (✔)
instance EncodableDynamicCharacter d => SimpleDynamicExtensionPostorderDecoration (DynamicDecorationDirectOptimizationPostorderResult d) d where

    extendDynamicToPostorder subDecoration localCost totalCost subTreeAvgLength ungapped gapped lhsAlignment rhsAlignment =
        DynamicDecorationDirectOptimizationPostorderResult
        { dynamicDecorationDirectOptimizationPostorderCharacterCost            = totalCost
        , dynamicDecorationDirectOptimizationPostorderCharacterLocalCost       = localCost
        , dynamicDecorationDirectOptimizationPostorderCharacterAverageLength   = subTreeAvgLength
        , dynamicDecorationDirectOptimizationPostorderEncodedField             = subDecoration ^. encoded
        , dynamicDecorationDirectOptimizationPostorderPreliminaryGappedField   = gapped
        , dynamicDecorationDirectOptimizationPostorderPreliminaryUngappedField = ungapped
        , dynamicDecorationDirectOptimizationPostorderLeftAlignmentField       = lhsAlignment
        , dynamicDecorationDirectOptimizationPostorderRightAlignmentField      = rhsAlignment
        }


-- | (✔)
instance Show d => ToXML (DynamicDecorationDirectOptimization d) where

    toXML decoration = xmlElement "Dynamic_DO_pre-order_decoration_result" attributes contents
        where
            attributes = []
            contents   = [ Left ("Local_cost"               , show (decoration ^. characterLocalCost) )
                         , Left ("Original_encoding"        , show (decoration ^. encoded)            )
                         , Left ("Preliminary_gapped_char"  , show (decoration ^. preliminaryGapped)  )
                         , Left ("Preliminary_ungapped_char", show (decoration ^. preliminaryUngapped))
                         , Left ("Final_gapped_char"        , show (decoration ^. finalGapped)        )
                         , Left ("Final_ungapped_char"      , show (decoration ^. finalUngapped)      )
                         ]


-- | (✔)
instance Show d => ToXML (DynamicDecorationDirectOptimizationPostorderResult d) where

    toXML decoration = xmlElement "Dynamic_DO_post-order_decoration_result" attributes contents
        where
            attributes = []
            contents   = [ Left ("Character_cost"           , show (decoration ^. characterCost)      )
                         , Left ("Local_cost"               , show (decoration ^. characterLocalCost) )
                         , Left ("Preliminary_gapped_char"  , show (decoration ^. preliminaryGapped)  )
                         , Left ("Preliminary_ungapped_char", show (decoration ^. preliminaryUngapped))
                         ]


renderCost
  :: ( HasCharacterCost s a
     , HasCharacterLocalCost s b
     , Show a
     , Show b
     )
  => s
  -> String
renderCost dec = unwords
      [ "Cost                 :"
      , show (dec ^. characterCost)
      , "{"
      , show (dec ^. characterLocalCost)
      , "}"
      ]

