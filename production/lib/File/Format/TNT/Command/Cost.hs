----------------------------------------------------------------------------
-- |
-- Module      :  File.Format.TNT.Command.Cost
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Parser for the COST command specifying custom TCM constructions for certain
-- chasracter indicies. 
----------------------------------------------------------------------------- 
{-# LANGUAGE FlexibleContexts, TypeFamilies #-}
module File.Format.TNT.Command.Cost where

import Data.Functor             (($>))
import Data.List.NonEmpty       (NonEmpty)
import Data.Matrix.NotStupid    (Matrix,matrix)
import Data.Maybe               (fromJust,fromMaybe)
import Data.Vector              ((!))
import File.Format.TNT.Internal
import Text.Megaparsec
import Text.Megaparsec.Custom   (double, nonEmpty)
import Text.Megaparsec.Prim     (MonadParsec)

-- | The attributes necessary for constructing a custom TCM.
--   Many 'TransitionCost' are expected to be folded together to form a TCm.
data TransitionCost
   = TransitionCost
   { origins   :: NonEmpty Char
   , symetric  :: Bool
   , terminals :: NonEmpty Char
   , costValue :: Double
   } deriving (Eq,Show)

-- | Parses a Cost command that consists of:
--
--  * A single specification of the character state change
--
--  * One or more character indicies or index ranges of affected characters
costCommand :: (MonadParsec e s m, Token s ~ Char) => m Cost
costCommand = costHeader *> costBody <* symbol (char ';')

-- | Consumes the superflous heading for a CCODE command.
costHeader :: (MonadParsec e s m, Token s ~ Char) => m ()
costHeader = symbol $ keyword "cost" 2

-- | The nonempty body of a COST command which represents the indicies to apply
--   a custom TCM to.
costBody :: (MonadParsec e s m, Token s ~ Char) => m Cost
costBody = do
      idx <- symbol characterIndicies
      _   <- symbol $ char '='
      transitions <- nonEmpty costDefinition
      pure . Cost idx $ condenseToMatrix transitions

-- | Fold over a nonmepty structure of 'Transition' costs to create a custom TCM.
condenseToMatrix :: (Foldable f, Functor f) => f TransitionCost -> Matrix Double
condenseToMatrix costs = matrix dimensions dimensions value
  where
    dimensions   = succ . fromJust $ maximumState `indexOf` discreteStateValues
    maximumState = maximum $ f <$> costs
      where
        f tc = max (maximum $ origins tc) (maximum $ terminals tc)
    value (i,j) = fromMaybe 1 $ foldl f Nothing costs
      where
        i' = discreteStateValues ! i
        j' = discreteStateValues ! j
        f m x
          | inject || (symetric x && surject) = Just $ costValue x
          | otherwise                         = m
          where
            inject  = i' `elem` origins x && j' `elem` terminals x
            surject = j' `elem` origins x && i' `elem` terminals x
    indexOf :: (Eq a, Foldable f) => a -> f a -> Maybe Int
    indexOf e = snd . foldl f (0, Nothing)
      where
        f a@(_, Just _ ) _ = a
        f   (i, Nothing) x
          | x == e    = (i  , Just i )
          | otherwise = (i+1, Nothing)

-- | Parses a 'TransitionCost' from within the body of a COST command.
--   Must contain a nonempty list of character state values and a transition
--   cost value. The transitional cost is interpreted as directed by default but
--   may optionally be specified as a symetric relation.
costDefinition :: (MonadParsec e s m, Token s ~ Char) => m TransitionCost
costDefinition = TransitionCost
             <$> symbol costStates
             <*> symbol costRelation
             <*> symbol costStates
             <*> symbol double
  where
    costRelation :: (MonadParsec e s m, Token s ~ Char) => m Bool
    costRelation = (char '>' $> False) <|> (char '/' $> True )
    costStates :: (MonadParsec e s m, Token s ~ Char) => m (NonEmpty Char)
    costStates = singleState <|> manyStates
      where
        singleState = pure <$> characterStateChar
        manyStates  = between open close (nonEmpty characterStateChar)
        open  = symbol $ char '['
        close = symbol $ char ']'

    
