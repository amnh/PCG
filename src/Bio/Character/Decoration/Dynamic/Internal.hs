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

{-# LANGUAGE DeriveGeneric, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, TypeFamilies #-}

{-# LANGUAGE UndecidableInstances #-}

module Bio.Character.Decoration.Dynamic.Internal
  ( DynamicDecorationDirectOptimization(..)
  , DynamicDecorationDirectOptimizationPostOrderResult(..)
  , DynamicDecorationImpliedAlignment(..)
  , DynamicDecorationInitial(..)
  ) where

import Bio.Character.Decoration.Dynamic.Class
import Bio.Character.Decoration.Shared
import Bio.Character.Encodable
import Bio.Metadata.CharacterName
import Bio.Metadata.Discrete
import Bio.Metadata.DiscreteWithTCM
import Bio.Metadata.Dynamic
import Control.DeepSeq
import Control.Lens
import Data.Alphabet
import Data.Bits
import Data.Foldable
import Data.Hashable
import Data.List.NonEmpty (intersperse)
import Data.MonoTraversable
import Data.Semigroup     (sconcat)
import Data.TopologyRepresentation
import GHC.Generics
import Text.XML


-- TODO: Make a polymorpic pre-order constructor.


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
data DynamicDecorationDirectOptimizationPostOrderResult d
   = DynamicDecorationDirectOptimizationPostOrderResult
   { dynamicDecorationDirectOptimizationPostOrderCharacterCost            :: {-# UNPACK #-} !Word
   , dynamicDecorationDirectOptimizationPostOrderCharacterLocalCost       :: {-# UNPACK #-} !Word
   , dynamicDecorationDirectOptimizationPostOrderCharacterAverageLength   :: {-# UNPACK #-} !AverageLength
   , dynamicDecorationDirectOptimizationPostOrderEncodedField             :: !d
   , dynamicDecorationDirectOptimizationPostOrderPreliminaryGappedField   :: !d
   , dynamicDecorationDirectOptimizationPostOrderPreliminaryUngappedField :: !d
   , dynamicDecorationDirectOptimizationPostOrderLeftAlignmentField       :: !d
   , dynamicDecorationDirectOptimizationPostOrderRightAlignmentField      :: !d
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
instance NFData d => NFData (DynamicDecorationDirectOptimizationPostOrderResult d)


-- | (✔)
instance NFData d => NFData (DynamicDecorationImpliedAlignment d)


-- | (✔)
instance EncodableDynamicCharacter d => DirectOptimizationDecoration (DynamicDecorationDirectOptimization d) d where


-- | (✔)
instance EncodableDynamicCharacter d => DirectOptimizationDecoration (DynamicDecorationImpliedAlignment d) d where


-- | (✔)
instance EncodableDynamicCharacter d => DirectOptimizationPostOrderDecoration (DynamicDecorationDirectOptimization d) d where


-- | (✔)
instance EncodableDynamicCharacter d => DirectOptimizationPostOrderDecoration (DynamicDecorationDirectOptimizationPostOrderResult d) d where


-- | (✔)
instance EncodableDynamicCharacter d => DirectOptimizationPostOrderDecoration (DynamicDecorationImpliedAlignment d) d where


-- | (✔)
instance (EncodableDynamicCharacter d) => DynamicCharacterDecoration (DynamicDecorationInitial d) d where

--    toDynamicCharacterDecoration :: CharacterName -> Double -> Alphabet String -> TCM -> (x -> a) -> x -> s
    toDynamicCharacterDecoration name weight alphabet scm g symbolSet =
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
instance HasAverageLength (DynamicDecorationDirectOptimizationPostOrderResult d) AverageLength where

    averageLength = lens dynamicDecorationDirectOptimizationPostOrderCharacterAverageLength (\e x -> e { dynamicDecorationDirectOptimizationPostOrderCharacterAverageLength = x })


-- | (✔)
instance HasCharacterCost (DynamicDecorationImpliedAlignment d) Word where

    characterCost = lens dynamicDecorationImpliedAlignmentCharacterCost (\e x -> e { dynamicDecorationImpliedAlignmentCharacterCost = x })


-- | (✔)
instance HasCharacterCost (DynamicDecorationDirectOptimization d) Word where

    characterCost = lens dynamicDecorationDirectOptimizationCharacterCost (\e x -> e { dynamicDecorationDirectOptimizationCharacterCost = x })


-- | (✔)
instance HasCharacterCost (DynamicDecorationDirectOptimizationPostOrderResult d) Word where

    characterCost = lens dynamicDecorationDirectOptimizationPostOrderCharacterCost (\e x -> e { dynamicDecorationDirectOptimizationPostOrderCharacterCost = x })


-- | (✔)
instance HasCharacterLocalCost (DynamicDecorationImpliedAlignment d) Word where

    characterLocalCost = lens dynamicDecorationImpliedAlignmentCharacterLocalCost (\e x -> e { dynamicDecorationImpliedAlignmentCharacterLocalCost = x })


-- | (✔)
instance HasCharacterLocalCost (DynamicDecorationDirectOptimization d) Word where

    characterLocalCost = lens dynamicDecorationDirectOptimizationCharacterLocalCost (\e x -> e { dynamicDecorationDirectOptimizationCharacterLocalCost = x })


-- | (✔)
instance HasCharacterLocalCost (DynamicDecorationDirectOptimizationPostOrderResult d) Word where

    characterLocalCost = lens dynamicDecorationDirectOptimizationPostOrderCharacterLocalCost (\e x -> e { dynamicDecorationDirectOptimizationPostOrderCharacterLocalCost = x })


-- | (✔)
instance HasEncoded (DynamicDecorationImpliedAlignment d) d where

    encoded = lens dynamicDecorationImpliedAlignmentEncodedField (\e x -> e { dynamicDecorationImpliedAlignmentEncodedField = x })


-- | (✔)
instance HasEncoded (DynamicDecorationDirectOptimization d) d where

    encoded = lens dynamicDecorationDirectOptimizationEncodedField (\e x -> e { dynamicDecorationDirectOptimizationEncodedField = x })


-- | (✔)
instance HasEncoded (DynamicDecorationDirectOptimizationPostOrderResult d) d where

    encoded = lens dynamicDecorationDirectOptimizationPostOrderEncodedField (\e x -> e { dynamicDecorationDirectOptimizationPostOrderEncodedField = x })


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
instance PossiblyMissingCharacter c => PossiblyMissingCharacter (DynamicDecorationDirectOptimizationPostOrderResult c) where

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
instance Hashable d => Hashable (DynamicDecorationDirectOptimizationPostOrderResult d) where

      hashWithSalt salt dec = foldr1 xor $
                              [ hashWithSalt salt . dynamicDecorationDirectOptimizationPostOrderCharacterCost
                              , hashWithSalt salt . dynamicDecorationDirectOptimizationPostOrderEncodedField
                              , hashWithSalt salt . dynamicDecorationDirectOptimizationPostOrderPreliminaryGappedField
                              , hashWithSalt salt . dynamicDecorationDirectOptimizationPostOrderPreliminaryUngappedField
                              , hashWithSalt salt . dynamicDecorationDirectOptimizationPostOrderLeftAlignmentField
                              , hashWithSalt salt . dynamicDecorationDirectOptimizationPostOrderRightAlignmentField
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
instance HasLeftAlignment (DynamicDecorationDirectOptimizationPostOrderResult d) d where

    leftAlignment = lens dynamicDecorationDirectOptimizationPostOrderLeftAlignmentField (\e x -> e { dynamicDecorationDirectOptimizationPostOrderLeftAlignmentField = x })


-- | (✔)
instance HasPreliminaryGapped (DynamicDecorationImpliedAlignment d) d where

    preliminaryGapped = lens dynamicDecorationImpliedAlignmentPreliminaryGappedField (\e x -> e { dynamicDecorationImpliedAlignmentPreliminaryGappedField = x })


-- | (✔)
instance HasPreliminaryGapped (DynamicDecorationDirectOptimization d) d where

    preliminaryGapped = lens dynamicDecorationDirectOptimizationPreliminaryGappedField (\e x -> e { dynamicDecorationDirectOptimizationPreliminaryGappedField = x })


-- | (✔)
instance HasPreliminaryGapped (DynamicDecorationDirectOptimizationPostOrderResult d) d where

    preliminaryGapped = lens dynamicDecorationDirectOptimizationPostOrderPreliminaryGappedField (\e x -> e { dynamicDecorationDirectOptimizationPostOrderPreliminaryGappedField = x })


-- | (✔)
instance HasPreliminaryUngapped (DynamicDecorationImpliedAlignment d) d where

    preliminaryUngapped = lens dynamicDecorationImpliedAlignmentPreliminaryUngappedField (\e x -> e { dynamicDecorationImpliedAlignmentPreliminaryUngappedField = x })


-- | (✔)
instance HasPreliminaryUngapped (DynamicDecorationDirectOptimization d) d where

    preliminaryUngapped = lens dynamicDecorationDirectOptimizationPreliminaryUngappedField (\e x -> e { dynamicDecorationDirectOptimizationPreliminaryUngappedField = x })


-- | (✔)
instance HasPreliminaryUngapped (DynamicDecorationDirectOptimizationPostOrderResult d) d where

    preliminaryUngapped = lens dynamicDecorationDirectOptimizationPostOrderPreliminaryUngappedField (\e x -> e { dynamicDecorationDirectOptimizationPostOrderPreliminaryUngappedField = x })


-- | (✔)
instance HasRightAlignment (DynamicDecorationImpliedAlignment d) d where

    rightAlignment = lens dynamicDecorationImpliedAlignmentLeftAlignmentField (\e x -> e { dynamicDecorationImpliedAlignmentLeftAlignmentField = x })


-- | (✔)
instance HasRightAlignment (DynamicDecorationDirectOptimization d) d where

    rightAlignment = lens dynamicDecorationDirectOptimizationRightAlignmentField (\e x -> e { dynamicDecorationDirectOptimizationRightAlignmentField = x })


-- | (✔)
instance HasRightAlignment (DynamicDecorationDirectOptimizationPostOrderResult d) d where

    rightAlignment = lens dynamicDecorationDirectOptimizationPostOrderRightAlignmentField (\e x -> e { dynamicDecorationDirectOptimizationPostOrderRightAlignmentField = x })


-- | (✔)
instance EncodableDynamicCharacter d => ImpliedAlignmentDecoration   (DynamicDecorationImpliedAlignment d) d where


-- | (✔)
instance EncodableDynamicCharacter d => PostOrderExtensionDirectOptimizationDecoration (DynamicDecorationDirectOptimization d) d where

    extendPostOrderToDirectOptimization subDecoration ungapped gapped single =
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
instance (EncodableStream d, Show d) => Show (DynamicDecorationDirectOptimizationPostOrderResult d) where

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
instance EncodableDynamicCharacter d => SimpleDynamicDecoration (DynamicDecorationDirectOptimizationPostOrderResult d) d where


-- | (✔)
instance EncodableDynamicCharacter d => SimpleDynamicDecoration (DynamicDecorationImpliedAlignment d) d where


-- | (✔)
instance EncodableDynamicCharacter d => SimpleDynamicDecoration (DynamicDecorationInitial d) d where


-- | (✔)
instance EncodableDynamicCharacter d => SimpleDynamicExtensionPostOrderDecoration (DynamicDecorationDirectOptimizationPostOrderResult d) d where

    extendDynamicToPostOrder subDecoration localCost totalCost subTreeAvgLength ungapped gapped lhsAlignment rhsAlignment =
        DynamicDecorationDirectOptimizationPostOrderResult
        { dynamicDecorationDirectOptimizationPostOrderCharacterCost            = totalCost
        , dynamicDecorationDirectOptimizationPostOrderCharacterLocalCost       = localCost
        , dynamicDecorationDirectOptimizationPostOrderCharacterAverageLength   = subTreeAvgLength
        , dynamicDecorationDirectOptimizationPostOrderEncodedField             = subDecoration ^. encoded
        , dynamicDecorationDirectOptimizationPostOrderPreliminaryGappedField   = gapped
        , dynamicDecorationDirectOptimizationPostOrderPreliminaryUngappedField = ungapped
        , dynamicDecorationDirectOptimizationPostOrderLeftAlignmentField       = lhsAlignment
        , dynamicDecorationDirectOptimizationPostOrderRightAlignmentField      = rhsAlignment
        }


-- | (✔)
instance (EncodableStream d, Show d) => ToXML (DynamicDecorationDirectOptimization d) where

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
instance (EncodableStream d, Show d) => ToXML (DynamicDecorationDirectOptimizationPostOrderResult d) where

    toXML decoration = xmlElement "Dynamic_DO_post-order_decoration_result" attributes contents
        where
            attributes = []
            contents   = [ Left ("Character_cost"           , show (decoration ^. characterCost)      )
                         , Left ("Local_cost"               , show (decoration ^. characterLocalCost) )
                         , Left ("Preliminary_gapped_char"  , show (decoration ^. preliminaryGapped)  )
                         , Left ("Preliminary_ungapped_char", show (decoration ^. preliminaryUngapped))
                         ]


-- |
-- Render a traversal foci to a String.
renderFoci :: TraversalFoci -> String
renderFoci foci = prefix <> body <> "\n"
  where
    prefix   = "Traversal Foci {" <> show (length foci) <> "}\n"
    body     = sconcat . intersperse "\n" $ fmap g foci
    g (e,te) = "  Traversal Focus Edge: " <> show e <> " with network edges in topology: " <> show (toList $ includedNetworkEdges te)


-- |
-- Generic rendering function for a dynamic character decoration with descriptive
-- fields for determining the result of a network traversal.
renderingDecorationContext
  :: ( HasCharacterAlphabet  s x
     , HasCharacterCost      s y
     , HasCharacterLocalCost s z
     , HasTraversalFoci      s (Maybe TraversalFoci)
     , Show x
     , Show y
     , Show z
     ) => s -> (String, String, String)
renderingDecorationContext dec = (shownAlphabet, shownCost, shownFoci)
  where
    shownAlphabet = show $ dec ^. characterAlphabet

    shownFoci = maybe "No Foci exist\n" renderFoci $ dec ^. traversalFoci

    shownCost = unwords
        [ "Cost                 :"
        , show (dec ^. characterCost)
        , "{"
        , show (dec ^. characterLocalCost)
        , "}"
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

