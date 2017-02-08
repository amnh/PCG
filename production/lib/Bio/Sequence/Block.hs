------------------------------------------------------------------------------
-- |
-- Module      :  Bio.Metadata.Sequence.Block
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}

module Bio.Sequence.Block
  ( CharacterBlock(..)
  , blockCost
  , toMissingCharacters
  , hexmap
  , hexTranspose
  , hexZipWith
  ) where


import           Bio.Character.Encodable
import           Bio.Character.Decoration.Continuous
import           Bio.Metadata.CharacterName
import           Bio.Sequence.Block.Internal
import           Control.Lens
import           Control.Parallel.Custom
import           Control.Parallel.Strategies
import           Data.Foldable
import           Data.Key
import           Data.Monoid                          (mappend)
import           Data.Semigroup
--import           Data.Semigroup.Traversable
import           Data.TCM
import           Data.Vector                          (Vector)
import           Data.Vector.Instances                ()
import qualified Data.Vector                   as V
import           Prelude                       hiding (zipWith)
import           Safe                                 (headMay)


-- |
-- Perform a six way map over the polymorphic types.
hexmap :: (m -> m')
       -> (i -> i')
       -> (c -> c')
       -> (f -> f')
       -> (a -> a')
       -> (d -> d')
       -> CharacterBlock m  i  c  f  a  d
       -> CharacterBlock m' i' c' f' a' d'
hexmap f1 f2 f3 f4 f5 f6 =
    CharacterBlock
      <$> (parmap rpar f3 . continuousCharacterBins )
      <*> (parmap rpar f4 . nonAdditiveCharacterBins)
      <*> (parmap rpar f5 . additiveCharacterBins   )
      <*> (parmap rpar f1 . metricCharacterBins     )
      <*> (parmap rpar f2 . nonMetricCharacterBins  )
      <*> (parmap rpar f6 . dynamicCharacters       )


hexTranspose :: Traversable t => t (CharacterBlock m i c f a d) -> CharacterBlock (t m) (t i) (t c) (t f) (t a) (t d)
hexTranspose = 
    CharacterBlock
      <$> transposition continuousCharacterBins
      <*> transposition nonAdditiveCharacterBins
      <*> transposition additiveCharacterBins
      <*> transposition metricCharacterBins
      <*> transposition nonMetricCharacterBins
      <*> transposition dynamicCharacters
  where
    transposition f xs =
        case maybe 0 length . headMay $ toList listOfVectors of
          0 -> mempty
          n -> V.generate n g
      where
        g i = (V.! i) <$> listOfVectors
        listOfVectors = fmap f xs


hexZipWith :: (m1 -> m2 -> m3)
           -> (i1 -> i2 -> i3) 
           -> (c1 -> c2 -> c3)
           -> (f1 -> f2 -> f3)
           -> (a1 -> a2 -> a3)
           -> (d1 -> d2 -> d3)
           -> CharacterBlock m1 i1 c1 f1 a1 d1
           -> CharacterBlock m2 i2 c2 f2 a2 d2
           -> CharacterBlock m3 i3 c3 f3 a3 d3
hexZipWith f1 f2 f3 f4 f5 f6 lhs rhs =
    CharacterBlock
        { continuousCharacterBins  = parZipWith rpar f3 (continuousCharacterBins  lhs) (continuousCharacterBins  rhs)
        , nonAdditiveCharacterBins = parZipWith rpar f4 (nonAdditiveCharacterBins lhs) (nonAdditiveCharacterBins rhs)
        , additiveCharacterBins    = parZipWith rpar f5 (additiveCharacterBins    lhs) (additiveCharacterBins    rhs)
        , metricCharacterBins      = parZipWith rpar f1 (metricCharacterBins      lhs) (metricCharacterBins      rhs)
        , nonMetricCharacterBins   = parZipWith rpar f2 (nonMetricCharacterBins   lhs) (nonMetricCharacterBins   rhs)
        , dynamicCharacters        =    zipWith      f6 (dynamicCharacters        lhs) (dynamicCharacters        rhs)
        }


-- |
-- Convert all characters contained in the block to thier missing value.
toMissingCharacters :: ( PossiblyMissingCharacter m
                       , PossiblyMissingCharacter i
                       , PossiblyMissingCharacter c
                       , PossiblyMissingCharacter f
                       , PossiblyMissingCharacter a
                       , PossiblyMissingCharacter d
                       )
                    => CharacterBlock m i c f a d
                    -> CharacterBlock m i c f a d
toMissingCharacters cb =
    CharacterBlock
    { continuousCharacterBins  = toMissing <$> continuousCharacterBins  cb
    , nonAdditiveCharacterBins = toMissing <$> nonAdditiveCharacterBins cb
    , additiveCharacterBins    = toMissing <$> additiveCharacterBins    cb
    , metricCharacterBins      = toMissing <$> metricCharacterBins      cb
    , nonMetricCharacterBins   = toMissing <$> nonMetricCharacterBins   cb
    , dynamicCharacters        = toMissing <$> dynamicCharacters        cb
    }


blockCost :: ( HasCharacterCost   m e
             , HasCharacterCost   i e
--             , HasCharacterCost   c Double
             , HasCharacterCost   f e
             , HasCharacterCost   a e
             , HasCharacterCost   d e
             , HasCharacterWeight m Double
             , HasCharacterWeight i Double
--             , HasCharacterWeight c Double
             , HasCharacterWeight f Double
             , HasCharacterWeight a Double
             , HasCharacterWeight d Double
             , Integral e
             )
          => CharacterBlock m i c f a d
          -> Double
blockCost block = sum . fmap sum $
    [ parmap rpar integralCost . nonAdditiveCharacterBins
--    , parmap rpar floatingCost . continuousCharacterBins 
    , parmap rpar integralCost . additiveCharacterBins   
    , parmap rpar integralCost . metricCharacterBins     
    , parmap rpar integralCost . nonMetricCharacterBins  
    , parmap rpar integralCost . dynamicCharacters       
    ] <*> [block]
  where
    integralCost dec = fromIntegral cost * weight
      where
        cost   = dec ^. characterCost
        weight = dec ^. characterWeight
{-
    floatingCost dec = cost * weight
      where
        cost   = dec ^. characterCost
        weight = dec ^. characterWeight
-}
