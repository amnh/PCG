------------------------------------------------------------------------------
-- |
-- Module      :  Bio.Metadata.Sequence.Block.Internal
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}

module Bio.Sequence.Block.Internal
  ( CharacterBlock(..)
  ) where


import Data.Foldable
import Data.Semigroup
import Data.Vector            (Vector)
import Data.Vector.Instances  ()
import Prelude         hiding (zipWith)
import Text.XML.Custom


-- |
-- Represents a block of charcters which are optimized atomically together across
-- networks. The 'CharacterBlock' is polymorphic over static and dynamic character
-- definitions.
--
-- Use '(<>)' to construct larger blocks.
data CharacterBlock u v w x y z
   = CharacterBlock
   { continuousCharacterBins  :: Vector u
   , nonAdditiveCharacterBins :: Vector v
   , additiveCharacterBins    :: Vector w
   , metricCharacterBins      :: Vector x
   , nonMetricCharacterBins   :: Vector y
   , dynamicCharacters        :: Vector z
   } deriving (Eq)


instance Semigroup (CharacterBlock u v w x y z) where

    lhs <> rhs =
        CharacterBlock
          { continuousCharacterBins  = continuousCharacterBins  lhs <> continuousCharacterBins  rhs
          , nonAdditiveCharacterBins = nonAdditiveCharacterBins lhs <> nonAdditiveCharacterBins rhs
          , additiveCharacterBins    = additiveCharacterBins    lhs <> additiveCharacterBins    rhs
          , metricCharacterBins      = metricCharacterBins      lhs <> metricCharacterBins      rhs
          , nonMetricCharacterBins   = nonMetricCharacterBins   lhs <> nonMetricCharacterBins   rhs
          , dynamicCharacters        = dynamicCharacters        lhs <> dynamicCharacters        rhs
          }


instance ( Show u
         , Show v
         , Show w
         , Show x
         , Show y
         , Show z
         ) => Show (CharacterBlock u v w x y z) where

    show block = unlines
        [ "Fitch Characters:"
        , niceRendering $ nonAdditiveCharacterBins block
        , "Additive Characters:"
        , niceRendering $ additiveCharacterBins block
        , "NonMetric Characters:"
        , niceRendering $ nonMetricCharacterBins block
        , "Continuous Characters: "
        , niceRendering $ continuousCharacterBins block
        , "Metric Characters:"
        , niceRendering $ metricCharacterBins block
        , "Dynamic Characters:"
        , niceRendering $ dynamicCharacters block
        ]
      where
        niceRendering :: (Foldable t, Show a) => t a -> String
        niceRendering = unlines . fmap (unlines . fmap ("  " <>) . lines . show) . toList


instance ( ToXML u
         , ToXML v
         , ToXML w
         , ToXML x
         , ToXML y
         , ToXML z
         ) => ToXML (CharacterBlock u v w x y z) where

    toXML block = xmlElement "Character block" attributes contents
        where
            attributes = []
            -- [(String,           Either String Text.XML.Light.Types.Element   )]
            -- [(String, [(String, Either String Text.XML.Light.Types.Element)] )]
            contents   = createXMLContent <$> [ ("Fitch Characters"     , toList (makeContentTuple <$> nonAdditiveCharacterBins block) )
                                              , ("Additive Characters"  , toList (makeContentTuple <$> additiveCharacterBins    block) )
                                              , ("NonMetric Characters" , toList (makeContentTuple <$> nonMetricCharacterBins   block) )
                                              , ("Continuous Characters", toList (makeContentTuple <$> continuousCharacterBins  block) )
                                              , ("Metric Characters"    , toList (makeContentTuple <$> nonMetricCharacterBins   block) )
                                              , ("Dynamic Characters"   , toList (makeContentTuple <$> dynamicCharacters        block) )
                                              ]
            makeContentTuple :: Vector a
            makeContentTuple bin = ("Char", Right toXML bin)

            -- createXMLContent :: [(String, Either String Text.XML.Light.Types.Element)] -> [Content]
            createXMLContent lst = parseTuple <$> lst