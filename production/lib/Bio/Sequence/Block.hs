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

{-# LANGUAGE ConstraintKinds, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}

module Bio.Sequence.Block
  ( CharacterBlock(..)
  , HasBlockCost
  , blockCost
  , toMissingCharacters
  , hexmap
  , hexTranspose
  , hexZipWith
  ) where


import           Bio.Character.Encodable
import           Bio.Character.Decoration.Continuous
import           Bio.Sequence.Block.Internal
import           Control.Lens
import           Control.Parallel.Custom
import           Control.Parallel.Strategies
import           Data.Foldable
import           Data.Key
import           Data.Semigroup
import           Data.Vector.Instances                ()
import qualified Data.Vector                   as V
import           Prelude                       hiding (zipWith)
import           Safe                                 (headMay)


-- |
-- CharacterBlocks satisfying this constraint have a calculable cost.
type HasBlockCost u v w x y z i r =
    ( HasCharacterCost   u i
    , HasCharacterCost   v i
    , HasCharacterCost   w r
    , HasCharacterCost   x i
    , HasCharacterCost   y i
    , HasCharacterCost   z i
    , HasCharacterWeight u r
    , HasCharacterWeight v r
    , HasCharacterWeight w r
    , HasCharacterWeight x r
    , HasCharacterWeight y r
    , HasCharacterWeight z r
    , Integral i
    , Real     r
    )


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


-- |
-- Performs a 2D transform on the 'Traversable' structure of 'CharacterBlock'
-- values.
-- 
-- Assumes that the 'CharacterBlock' values in the 'Traversable' structure are of
-- equal length. If this assumtion is violated, the result will be truncated.
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


-- |
-- Performs a zip over the two character blocks. Uses the input functions to zip
-- the different character types in the character block.
-- 
-- Assumes that the 'CharacterBlock' values have the same number of each character
-- type. If this assumtion is violated, the result will be truncated.
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


-- |
-- Calculates the cost of a 'CharacterBlock'. Performs some of the operation in
-- parallel.
blockCost :: HasBlockCost u v w x y z i r => CharacterBlock u v w x y z -> r
blockCost block = sum . fmap sum $
    [ parmap rpar integralCost . nonAdditiveCharacterBins
    , parmap rpar floatingCost . continuousCharacterBins 
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

    floatingCost dec = cost * weight
      where
        cost   = dec ^. characterCost
        weight = dec ^. characterWeight

