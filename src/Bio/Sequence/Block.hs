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
  , HasRootCost
  , blockCost
  , rootCost
  , staticCost
  , toMissingCharacters
  , hexmap
  , hexTranspose
  , hexZipWith
  ) where


import           Bio.Character.Encodable
import           Bio.Character.Decoration.Continuous
import           Bio.Character.Decoration.Discrete
import           Bio.Character.Decoration.Dynamic
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


-- |
-- CharacterBlocks satisfying this constraint have a calculable cost.
type HasBlockCost u v w x y z i r =
    ( HasCharacterCost   u r
    , HasCharacterCost   v i
    , HasCharacterCost   w i
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
-- CharacterBlocks satisfying this constraint have a calculable cost.
type HasRootCost u v w x y z r =
    ( HasCharacterWeight   u r
    , HasCharacterWeight   v r
    , HasCharacterWeight   w r
    , HasCharacterWeight   x r
    , HasCharacterWeight   y r
    , HasCharacterWeight   z r
    , HasAverageLength     z AverageLength
    , PossiblyMissingCharacter u
    , PossiblyMissingCharacter v
    , PossiblyMissingCharacter w
    , PossiblyMissingCharacter x
    , PossiblyMissingCharacter y
    , PossiblyMissingCharacter z
    , Floating r
    , Real     r
    )


-- |
-- Perform a six way map over the polymorphic types.
hexmap :: (u -> u')
       -> (v -> v')
       -> (w -> w')
       -> (x -> x')
       -> (y -> y')
       -> (z -> z')
       -> CharacterBlock u  v  w  x  y  z
       -> CharacterBlock u' v' w' x' y' z'
hexmap f1 f2 f3 f4 f5 f6 =
    CharacterBlock
      <$> (parmap rpar f1 . continuousCharacterBins )
      <*> (parmap rpar f2 . nonAdditiveCharacterBins)
      <*> (parmap rpar f3 . additiveCharacterBins   )
      <*> (parmap rpar f4 . metricCharacterBins     )
      <*> (parmap rpar f5 . nonMetricCharacterBins  )
      <*> (parmap rpar f6 . dynamicCharacters       )


-- |
-- Performs a 2D transform on the 'Traversable' structure of 'CharacterBlock'
-- values.
-- 
-- Assumes that the 'CharacterBlock' values in the 'Traversable' structure are of
-- equal length. If this assumtion is violated, the result will be truncated.
hexTranspose :: Traversable t => t (CharacterBlock u v w x y z) -> CharacterBlock (t u) (t v) (t w) (t x) (t y) (t z)
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
        case toList listOfVectors of
          [] -> mempty
          ys -> V.generate (length ys) g
      where
        g i = (V.! i) <$> listOfVectors
        listOfVectors = fmap f xs


-- |
-- Performs a zip over the two character blocks. Uses the input functions to zip
-- the different character types in the character block.
-- 
-- Assumes that the 'CharacterBlock' values have the same number of each character
-- type. If this assumtion is violated, the result will be truncated.
hexZipWith :: (u -> u' -> u'')
           -> (v -> v' -> v'') 
           -> (w -> w' -> w'')
           -> (x -> x' -> x'')
           -> (y -> y' -> y'')
           -> (z -> z' -> z'')
           -> CharacterBlock u   v   w   x   y   z
           -> CharacterBlock u'  v'  w'  x'  y'  z'
           -> CharacterBlock u'' v'' w'' x'' y'' z''
hexZipWith f1 f2 f3 f4 f5 f6 lhs rhs =
    CharacterBlock
        { continuousCharacterBins  = parZipWith rpar f1 (continuousCharacterBins  lhs) (continuousCharacterBins  rhs)
        , nonAdditiveCharacterBins = parZipWith rpar f2 (nonAdditiveCharacterBins lhs) (nonAdditiveCharacterBins rhs)
        , additiveCharacterBins    = parZipWith rpar f3 (additiveCharacterBins    lhs) (additiveCharacterBins    rhs)
        , metricCharacterBins      = parZipWith rpar f4 (metricCharacterBins      lhs) (metricCharacterBins      rhs)
        , nonMetricCharacterBins   = parZipWith rpar f5 (nonMetricCharacterBins   lhs) (nonMetricCharacterBins   rhs)
        , dynamicCharacters        =    zipWith      f6 (dynamicCharacters        lhs) (dynamicCharacters        rhs)
        }


-- |
-- Convert all characters contained in the block to thier missing value.
toMissingCharacters :: ( PossiblyMissingCharacter u
                       , PossiblyMissingCharacter v
                       , PossiblyMissingCharacter w
                       , PossiblyMissingCharacter x
                       , PossiblyMissingCharacter y 
                       , PossiblyMissingCharacter z
                       )
                    => CharacterBlock u v w x y z
                    -> CharacterBlock u v w x y z
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
    [ parmap rpar floatingCost . continuousCharacterBins 
    , parmap rpar integralCost . nonAdditiveCharacterBins
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


rootCost :: (HasRootCost u v w x y z r, Integral i) => i -> CharacterBlock u v w x y z -> r
rootCost rootCount block = rootMultiplier . sum . fmap sum $
    [ parmap rpar staticRootCost  . continuousCharacterBins 
    , parmap rpar staticRootCost  . nonAdditiveCharacterBins
    , parmap rpar staticRootCost  . additiveCharacterBins
    , parmap rpar staticRootCost  . metricCharacterBins     
    , parmap rpar staticRootCost  . nonMetricCharacterBins  
    , parmap rpar dynamicRootCost . dynamicCharacters       
    ] <*> [block]
  where
    rootMultiplier x = (otherRoots * x) / 2
      where
        otherRoots = max 0 (fromIntegral rootCount - 1)
    
    staticRootCost dec
      | isMissing dec = 0
      | otherwise     = weight
      where
        weight = dec ^. characterWeight

    dynamicRootCost dec
      | isMissing dec = 0
      | otherwise     = weight * getAverageLength avgLen
      where
        avgLen = dec ^. averageLength
        weight = dec ^. characterWeight


-- |
-- Calculates the cost of a 'CharacterBlock'. Performs some of the operation in
-- parallel.
staticCost :: HasBlockCost u v w x y z i r => CharacterBlock u v w x y z -> r
staticCost block = sum . fmap sum $
    [ parmap rpar floatingCost . continuousCharacterBins 
    , parmap rpar integralCost . nonAdditiveCharacterBins
    , parmap rpar integralCost . additiveCharacterBins   
    , parmap rpar integralCost . metricCharacterBins     
    , parmap rpar integralCost . nonMetricCharacterBins  
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

